/*****************************************************************************\
 *  $Id: ipmipower_ping.c,v 1.14 2006-03-07 07:25:59 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
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
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#if STDC_HEADERS
#include <string.h>
#endif
#include <errno.h>

#include "ipmipower_ping.h"
#include "ipmipower_util.h"
#include "ipmipower_wrappers.h"

extern struct ipmipower_config *conf;
extern struct ipmipower_connection *ics;

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
  struct timeval cur_time;
    
  assert(timeout != NULL);

  if (conf->hosts == NULL)
    return;

  if (!conf->ping_interval_len)
    return;

  Gettimeofday(&cur_time, NULL);
  if (millisec_gt(&cur_time, &next_ping_sends_time) || force_discovery_sweep) 
    {
      force_discovery_sweep = 0;
      millisec_add(&cur_time, &next_ping_sends_time, conf->ping_interval_len);
      send_pings_flag++;
    }

  for (i = 0; i < conf->hosts_count; i++) 
    {
      int len;
      char buffer[RMCP_PACKET_BUFLEN];
      
      if (send_pings_flag) 
        {
          fiid_obj_t rmcp_hdr = NULL;
          fiid_obj_t rmcp_ping = NULL;
          int len;
          
          memset(buffer, '\0', RMCP_PACKET_BUFLEN);

          /* deal with packet heuristics */
          if (conf->ping_packet_count && conf->ping_percent) 
            {
              if (ics[i].ping_packet_count_send == conf->ping_packet_count) 
                {
                  if ((((double)(ics[i].ping_packet_count_send - ics[i].ping_packet_count_recv))/ics[i].ping_packet_count_send) > ((double)conf->ping_percent/100))
                    ics[i].link_state = LINK_BAD;
                  else
                    ics[i].link_state = LINK_GOOD;
                  
                  ics[i].ping_packet_count_send = 0;
                  ics[i].ping_packet_count_recv = 0;
                }
            }
            
          if (conf->ping_consec_count) 
            {
              if (ics[i].ping_last_packet_recv_flag == 0)
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
            err_exit("fill_rmcp_hdr_asf: %s", strerror(errno));

          if (fill_cmd_asf_presence_ping((ics[i].ping_sequence_number_counter % 
                                          (RMCP_ASF_MESSAGE_TAG_MAX + 1)), 
                                         rmcp_ping) < 0)
            err_exit("fill_cmd_asf_presence_ping: %s", strerror(errno));
            
          if ((len = assemble_rmcp_pkt(rmcp_hdr,
				       rmcp_ping, 
                                       (uint8_t *)buffer, 
				       RMCP_PACKET_BUFLEN)) < 0)
            err_exit("assemble_rmcp_pkt: %s", strerror(errno));
          
#ifndef NDEBUG
          if (conf->rmcpdump) 
            {
              char *hdr = 
                "============================================\n"
                "= RMCP Ping                                =\n"
                "============================================";
              Ipmi_dump_rmcp_packet(STDERR_FILENO, 
                                    ics[i].hostname, 
                                    hdr, 
                                    (uint8_t *)buffer, 
                                    len, 
                                    tmpl_cmd_asf_presence_ping);
            }
#endif /* NDEBUG */

            Cbuf_write(ics[i].ping_out, buffer, len);
            
            ics[i].last_ping_send.tv_sec = cur_time.tv_sec;
            ics[i].last_ping_send.tv_usec = cur_time.tv_usec;
            
            if (conf->ping_packet_count && conf->ping_percent)
              ics[i].ping_packet_count_send++;
            
            Fiid_obj_destroy(rmcp_hdr);
            Fiid_obj_destroy(rmcp_ping);
        }
      
      /* Did we receive something? */
      memset(buffer, '\0', RMCP_PACKET_BUFLEN);
      len = Cbuf_peek_and_drop(ics[i].ping_in, buffer, RMCP_PACKET_BUFLEN);
      if (len > 0) 
        {
          fiid_obj_t rmcp_hdr = NULL;
          fiid_obj_t rmcp_pong = NULL;
          uint64_t message_type, ipmi_supported;
          
          rmcp_hdr = Fiid_obj_create(tmpl_rmcp_hdr);
          rmcp_pong = Fiid_obj_create(tmpl_cmd_asf_presence_pong);
            
#ifndef NDEBUG
          if (conf->rmcpdump) 
            {
              char *hdr = 
                "============================================\n"
                "= RMCP Pong                                =\n"
                "============================================";
              Ipmi_dump_rmcp_packet(STDERR_FILENO, 
                                    ics[i].hostname, 
                                    hdr, 
                                    (uint8_t *)buffer, 
                                    len, 
                                    tmpl_cmd_asf_presence_pong);
            }
#endif /* NDEBUG */

          if (unassemble_rmcp_pkt(buffer, 
				  len, 
                                  rmcp_hdr, 
				  rmcp_pong) < 0)
            err_exit("unassemble_rmcp_pkt: %s", strerror(errno));
          
          /* achu: check for ipmi_support and pong type, but don't
           * check for message tag.  On occassion, I have witnessed
           * BMCs send message tags "out of sync".  For example, you
           * send 8, BMC returns 7.  You send 9, BMC returns 8.  We
           * really don't care if the BMC is out of sync.  We just
           * need to make sure we get something back from the BMC to
           * ensure the machine is still there.
           */

          Fiid_obj_get(rmcp_pong, (uint8_t *)"message_type", &message_type);
          Fiid_obj_get(rmcp_pong, 
		       (uint8_t *)"supported_entities.ipmi_supported", 
		       &ipmi_supported);
                      
          if (message_type == RMCP_ASF_MESSAGE_TYPE_PRESENCE_PONG && ipmi_supported) 
            {
              if (conf->ping_packet_count && conf->ping_percent)
                ics[i].ping_packet_count_recv++;
              
              if (conf->ping_consec_count) 
                {
                  /* Don't increment twice, its possible a previous pong
                   * response was late, and we quickly receive two
                   * pong responses
                   */
                  if (ics[i].ping_last_packet_recv_flag == 0)
                    ics[i].ping_consec_count++;
                  
                  ics[i].ping_last_packet_recv_flag++;
                }
              
              if (conf->ping_packet_count && conf->ping_percent) 
                {
                  if (ics[i].link_state == LINK_GOOD)
                    ics[i].discover_state = STATE_DISCOVERED;
                  else 
                    {
                      if (conf->ping_consec_count
                          && ics[i].ping_consec_count >= conf->ping_consec_count)
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
      if (millisec_diff(&cur_time, &ics[i].last_ping_recv) >= conf->ping_timeout_len)
        ics[i].discover_state = STATE_UNDISCOVERED;
    }

  *timeout = millisec_diff(&next_ping_sends_time, &cur_time);
}
