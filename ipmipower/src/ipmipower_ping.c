/*****************************************************************************\
 *  $Id: ipmipower_ping.c,v 1.39 2008-08-12 18:14:41 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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
#include <assert.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "ipmipower_ping.h"
#include "ipmipower_util.h"
#include "ipmipower_wrappers.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

extern struct ipmipower_arguments cmd_args;
extern struct ipmipower_connection *ics;
extern unsigned int ics_len;

/* next_ping_sends_time, when the next round of pings should be sent */
static struct timeval next_ping_sends_time;

/* force discovery sweep when user reconfigures hostnames */
static int force_discovery_sweep;

void
ipmipower_ping_force_discovery_sweep() 
{
  force_discovery_sweep = 1;
}

void 
ipmipower_ping_process_pings(int *timeout) 
{
  int i, send_pings_flag = 0;
  struct timeval cur_time, result;
  unsigned int ms_time;
    
  assert(timeout);

  if (!cmd_args.common.hostname)
    return;

  if (!cmd_args.ping_interval)
    return;

  Gettimeofday(&cur_time, NULL);
  if (timeval_gt(&cur_time, &next_ping_sends_time) || force_discovery_sweep) 
    {
      force_discovery_sweep = 0;
      timeval_add_ms(&cur_time, cmd_args.ping_interval, &next_ping_sends_time);
      send_pings_flag++;
    }

  for (i = 0; i < ics_len; i++) 
    {
      int len;
      char buffer[IPMIPOWER_PACKET_BUFLEN];
      
      if (send_pings_flag) 
        {
          fiid_obj_t rmcp_hdr = NULL;
          fiid_obj_t rmcp_ping = NULL;
          int len;
          
          memset(buffer, '\0', IPMIPOWER_PACKET_BUFLEN);

          /* deal with packet heuristics */
          if (cmd_args.ping_packet_count && cmd_args.ping_percent) 
            {
              if (ics[i].ping_packet_count_send == cmd_args.ping_packet_count) 
                {
                  if ((((double)(ics[i].ping_packet_count_send - ics[i].ping_packet_count_recv))/ics[i].ping_packet_count_send) > ((double)cmd_args.ping_percent/100))
                    ics[i].link_state = LINK_BAD;
                  else
                    ics[i].link_state = LINK_GOOD;
                  
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

	  rmcp_hdr = Fiid_obj_create(tmpl_rmcp_hdr);
	  rmcp_ping = Fiid_obj_create(tmpl_cmd_asf_presence_ping);
          
          if (fill_rmcp_hdr_asf(rmcp_hdr) < 0)
            ierr_exit("fill_rmcp_hdr_asf: %s", strerror(errno));

          if (fill_cmd_asf_presence_ping((ics[i].ping_sequence_number_counter % 
                                          (RMCP_ASF_MESSAGE_TAG_MAX + 1)), 
                                         rmcp_ping) < 0)
            ierr_exit("fill_cmd_asf_presence_ping: %s", strerror(errno));
            
          if ((len = assemble_rmcp_pkt(rmcp_hdr,
				       rmcp_ping, 
                                       (uint8_t *)buffer, 
				       IPMIPOWER_PACKET_BUFLEN)) < 0)
            ierr_exit("assemble_rmcp_pkt: %s", strerror(errno));
          
#ifndef NDEBUG
          if (cmd_args.rmcpdump) 
            {
              char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

              debug_hdr_str(DEBUG_UTIL_TYPE_NONE,
                            DEBUG_UTIL_DIRECTION_NONE,
                            DEBUG_UTIL_RMCPPING_STR,
                            hdrbuf,
                            DEBUG_UTIL_HDR_BUFLEN);

              if (ipmi_dump_rmcp_packet(STDERR_FILENO, 
                                        ics[i].hostname,
                                        hdrbuf, 
                                        NULL,
                                        (uint8_t *)buffer, 
                                        (uint32_t)len, 
                                        tmpl_cmd_asf_presence_ping) < 0)
                ierr_dbg("ipmi_dump_rmcp_packet: %s", strerror(errno));
            }
#endif /* NDEBUG */

            Cbuf_write(ics[i].ping_out, buffer, len);
            
            ics[i].last_ping_send.tv_sec = cur_time.tv_sec;
            ics[i].last_ping_send.tv_usec = cur_time.tv_usec;
            
            if (cmd_args.ping_packet_count && cmd_args.ping_percent)
              ics[i].ping_packet_count_send++;
            
            Fiid_obj_destroy(rmcp_hdr);
            Fiid_obj_destroy(rmcp_ping);
        }
      
      /* Did we receive something? */
      memset(buffer, '\0', IPMIPOWER_PACKET_BUFLEN);
      len = Cbuf_peek_and_drop(ics[i].ping_in, buffer, IPMIPOWER_PACKET_BUFLEN);
      if (len > 0) 
        {
          fiid_obj_t rmcp_hdr = NULL;
          fiid_obj_t rmcp_pong = NULL;
          uint64_t message_type, ipmi_supported;
          
          rmcp_hdr = Fiid_obj_create(tmpl_rmcp_hdr);
          rmcp_pong = Fiid_obj_create(tmpl_cmd_asf_presence_pong);
            
#ifndef NDEBUG
          if (cmd_args.rmcpdump) 
            {
              char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

              debug_hdr_str(DEBUG_UTIL_TYPE_NONE,
                            DEBUG_UTIL_DIRECTION_NONE,
                            DEBUG_UTIL_RMCPPING_STR,
                            hdrbuf,
                            DEBUG_UTIL_HDR_BUFLEN);

              if (ipmi_dump_rmcp_packet(STDERR_FILENO, 
                                        ics[i].hostname,
                                        hdrbuf, 
                                        NULL,
                                        (uint8_t *)buffer, 
                                        (uint32_t)len, 
                                        tmpl_cmd_asf_presence_pong) < 0)
                ierr_dbg("ipmi_dump_rmcp_packet: %s", strerror(errno));
            }
#endif /* NDEBUG */

          if (unassemble_rmcp_pkt((uint8_t *)buffer, 
				  len, 
                                  rmcp_hdr, 
				  rmcp_pong) < 0)
            ierr_exit("unassemble_rmcp_pkt: %s", strerror(errno));
          
          /* achu: check for ipmi_support and pong type, but don't
           * check for message tag.  On occassion, I have witnessed
           * BMCs send message tags "out of sync".  For example, you
           * send 8, BMC returns 7.  You send 9, BMC returns 8.  We
           * really don't care if the BMC is out of sync.  We just
           * need to make sure we get something back from the BMC to
           * ensure the machine is still there.
           */

          Fiid_obj_get(rmcp_pong, "message_type", &message_type);
          Fiid_obj_get(rmcp_pong, 
		       "supported_entities.ipmi_supported", 
		       &ipmi_supported);
                      
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
                  if (ics[i].link_state == LINK_GOOD)
                    ics[i].discover_state = STATE_DISCOVERED;
                  else 
                    {
                      if (cmd_args.ping_consec_count
                          && ics[i].ping_consec_count >= cmd_args.ping_consec_count)
                        ics[i].discover_state = STATE_DISCOVERED;
                      else
                        ics[i].discover_state = STATE_BADCONNECTION;
                    }
                }
              else 
                {
                  ics[i].discover_state = STATE_DISCOVERED;
                }
              ics[i].last_ping_recv.tv_sec = cur_time.tv_sec;
              ics[i].last_ping_recv.tv_usec = cur_time.tv_usec;
              
              Fiid_obj_destroy(rmcp_hdr);
              Fiid_obj_destroy(rmcp_pong);
            }
        }
      
      /* Is the node gone?? */
      timeval_sub(&cur_time, &ics[i].last_ping_recv, &result);
      timeval_millisecond_calc(&result, &ms_time);
      if (ms_time >= cmd_args.ping_timeout)
        ics[i].discover_state = STATE_UNDISCOVERED;
    }

  timeval_sub(&next_ping_sends_time, &cur_time, &result);
  timeval_millisecond_calc(&result, &ms_time);
  *timeout = (int)ms_time;
}
