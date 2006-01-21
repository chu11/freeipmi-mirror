/*****************************************************************************\
 *  $Id: rmcpping.c,v 1.4.2.1 2006-01-21 09:17:22 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155448
 *
 *  This file is part of Ipmiping, tools for pinging IPMI and RMCP compliant
 *  remote systems. For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiping is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiping is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiping; if not, write to the Free Software Foundation, Inc.,
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#include <errno.h>
#include <assert.h>
#include "freeipmi.h"

#define _supported(x)   (x) ? "supported" : "not-supported"

/* Avoid use of message tag number 0xFF.  Behavior of message tag 0xFF
 * is unpredictable.  See IPMI 1.5 Specification and DMTF RMCP
 * specification for details.
 */
#define RMCP_MSG_TAG_MAX  0xFE

void *
Fiid_obj_calloc(fiid_template_t tmpl)
{
  void *ptr;
 
  assert(tmpl != NULL);
 
  if ((ptr = fiid_obj_calloc(tmpl)) == NULL)
    ipmi_ping_err_exit("fiid_obj_calloc: %s", strerror(errno));
 
  return ptr;
}
 
void
Fiid_obj_free(fiid_obj_t obj)
{
  fiid_obj_free(obj);
}

void
Fiid_obj_get(fiid_obj_t obj, fiid_template_t tmpl,
             uint8_t *field, uint64_t *val)
{
  assert(obj != NULL && tmpl != NULL && field != NULL && val != NULL);

  if (fiid_obj_get(obj, tmpl, field, val) < 0)
    ipmi_ping_err_exit("fiid_obj_get: %s", strerror(errno));
}

int 
createpacket(char *buffer, 
             int buflen, 
             unsigned int seq_num, 
             int debug)
{
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_rmcp_cmd = NULL;
  int len;

  assert(buffer != NULL);
    
  if (buflen < 0)
    return -1;

  if (buflen == 0)
    return 0;

  obj_rmcp_hdr = fiid_obj_calloc(tmpl_hdr_rmcp);
  obj_rmcp_cmd = fiid_obj_calloc(tmpl_cmd_asf_presence_ping);

  if (fill_hdr_rmcp_asf(obj_rmcp_hdr) < 0)
    ipmi_ping_err_exit("fill_hdr_rmcp_asf: %s", strerror(errno));

  if (fill_cmd_asf_presence_ping(seq_num % (RMCP_MSG_TAG_MAX + 1), 
                                 obj_rmcp_cmd) < 0)
    ipmi_ping_err_exit("fill_cmd_asf_presence_ping: %s", strerror(errno));

  if ((len = assemble_rmcp_pkt(obj_rmcp_hdr, obj_rmcp_cmd, 
                               tmpl_cmd_asf_presence_ping,
                               (uint8_t *)buffer, buflen)) < 0)
    ipmi_ping_err_exit("assemble_rmcp_pkt: %s", strerror(errno));
  
#ifndef NDEBUG
  if (debug)
    {
      if (fiid_obj_dump_rmcp(STDERR_FILENO, "Ping", NULL, 
                             (uint8_t *)buffer, len, 
                             tmpl_cmd_asf_presence_ping) < 0)
        ipmi_ping_err_exit("fiid_obj_dump_lan: %s", strerror(errno));
    }
#endif

  Fiid_obj_free(obj_rmcp_hdr);
  Fiid_obj_free(obj_rmcp_cmd);
  return len;
}

int 
parsepacket(char *buffer, 
            int buflen, 
            const char *from, 
            unsigned int seq_num, 
            int verbose, 
            int debug) 
{
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_rmcp_cmd = NULL;
  uint64_t msg_type, ipmi_supported, msg_tag;
  int retval = -1;

  assert(buffer != NULL && from != NULL);
  
  if (buflen == 0)
    return 0;

  obj_rmcp_hdr = fiid_obj_calloc(tmpl_hdr_rmcp);
  obj_rmcp_cmd = fiid_obj_calloc(tmpl_cmd_asf_presence_pong);

#ifndef NDEBUG
  if (debug)
    {
      if (fiid_obj_dump_rmcp(STDERR_FILENO, "Pong", NULL, buffer, buflen, 
                             tmpl_cmd_asf_presence_pong) < 0)
        ipmi_ping_err_exit("fiid_obj_dump_lan: %s", strerror(errno));
    }
#endif

  if (unassemble_rmcp_pkt(buffer, buflen, tmpl_cmd_asf_presence_pong, 
                          obj_rmcp_hdr, obj_rmcp_cmd) < 0)
    ipmi_ping_err_exit("unassemble_rmcp_pkt: %s", strerror(errno));

  Fiid_obj_get(obj_rmcp_cmd, tmpl_cmd_asf_presence_pong, 
               (uint8_t *)"msg_type", (uint64_t *)&msg_type);

  if (msg_type != RMCP_ASF_MSG_TYPE_PRESENCE_PONG)
    {
      retval = 0;
      goto cleanup;
    }

  Fiid_obj_get(obj_rmcp_cmd, tmpl_cmd_asf_presence_pong, 
               (uint8_t *)"msg_tag", (uint64_t *)&msg_tag);
  if (msg_tag != (seq_num % (RMCP_MSG_TAG_MAX + 1)))
    {
      retval = 0;
      goto cleanup;
    }

  printf("pong received from %s: msg_tag=%u", from, (uint32_t)msg_tag);
  if (verbose)
    {
      Fiid_obj_get(obj_rmcp_cmd, tmpl_cmd_asf_presence_pong, 
                   (uint8_t *)"supported_entities.ipmi_supported", 
                   (uint64_t *)&ipmi_supported);
      printf(", ipmi %s", _supported(ipmi_supported));
    }
  printf("\n");
  
  retval = 1;
 cleanup:
  fiid_obj_free(obj_rmcp_hdr);
  fiid_obj_free(obj_rmcp_cmd);
  return retval;
}

void 
latepacket(unsigned int seq_num) 
{
  printf("pong timed out: msg_tag=%u\n", seq_num % (RMCP_MSG_TAG_MAX + 1));
}

int
endresult(const char *progname, 
          const char *destination,
          unsigned int sent_count, 
          unsigned int recv_count) 
{
  double percent = 0;

  assert(progname != NULL && destination != NULL); 

  if (sent_count > 0)
    percent = ((double)(sent_count - recv_count)/sent_count)*100;

  printf("--- %s %s statistics ---\n", progname, destination);
  printf("%d pings transmitted, %d pongs received in time, "
         "%2.1f%% packet loss\n",
         sent_count, recv_count, percent);

  return ((recv_count > 0) ? 0 : 1);
}

int 
main(int argc, char **argv) 
{
#ifndef NDEBUG
  ipmi_ping_setup(argc, argv, 0, RMCP_MSG_TAG_MAX, "hVc:i:I:t:vs:d");
#else
  ipmi_ping_setup(argc, argv, 0, RMCP_MSG_TAG_MAX, "hVc:i:I:t:vs:");
#endif
  ipmi_ping_loop(createpacket, parsepacket, latepacket, endresult);
  exit(1);                    /* NOT REACHED */
}
