/*****************************************************************************\
 *  $Id: rmcpping.c,v 1.18 2006-03-07 21:57:15 chu11 Exp $
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
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-ping.h"

#define _supported(x)   (x) ? "supported" : "not-supported"

static fiid_obj_t
_fiid_obj_create(fiid_template_t tmpl)
{
  fiid_obj_t obj;
 
  assert(tmpl != NULL);
 
  if ((obj = fiid_obj_create(tmpl)) == NULL)
    ipmi_ping_err_exit("fiid_obj_create: %s", strerror(errno));
 
  return obj;
}
 
static void
_fiid_obj_destroy(fiid_obj_t obj)
{
  fiid_obj_destroy(obj);
}

static void
_fiid_obj_get(fiid_obj_t obj, char *field, uint64_t *val)
{
  assert(obj && field && val);

  if (fiid_obj_get(obj, field, val) < 0)
    ipmi_ping_err_exit("fiid_obj_get: %s", strerror(errno));
}

int 
createpacket(char *buffer, 
             int buflen, 
             unsigned int sequence_number, 
             int version,
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

  obj_rmcp_hdr = _fiid_obj_create(tmpl_rmcp_hdr);
  obj_rmcp_cmd = _fiid_obj_create(tmpl_cmd_asf_presence_ping);

  if (fill_rmcp_hdr_asf(obj_rmcp_hdr) < 0)
    ipmi_ping_err_exit("fill_rmcp_hdr_asf: %s", strerror(errno));

  /* Avoid use of message tag number 0xFF.  Behavior of message tag 0xFF
   * is unpredictable.  See IPMI 1.5 Specification and DMTF RMCP
   * specification for details.
   */

  if (fill_cmd_asf_presence_ping(sequence_number % (RMCP_ASF_MESSAGE_TAG_MAX + 1), 
                                 obj_rmcp_cmd) < 0)
    ipmi_ping_err_exit("fill_cmd_asf_presence_ping: %s", strerror(errno));

  if ((len = assemble_rmcp_pkt(obj_rmcp_hdr, obj_rmcp_cmd, 
                               (uint8_t *)buffer, buflen)) < 0)
    ipmi_ping_err_exit("assemble_rmcp_pkt: %s", strerror(errno));
  
#ifndef NDEBUG
  if (debug)
    {
      if (ipmi_dump_rmcp_packet(STDERR_FILENO, 
                                "Ping", 
                                NULL, 
                                (uint8_t *)buffer, 
                                (uint32_t)len, 
                                tmpl_cmd_asf_presence_ping) < 0)
        ipmi_ping_err_exit("ipmi_dump_rmcp_packet: %s", strerror(errno));
    }
#endif

  _fiid_obj_destroy(obj_rmcp_hdr);
  _fiid_obj_destroy(obj_rmcp_cmd);
  return len;
}

int 
parsepacket(char *buffer, 
            int buflen, 
            const char *from, 
            unsigned int sequence_number, 
            int verbose, 
            int version,
            int debug) 
{
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_rmcp_cmd = NULL;
  uint64_t message_type, ipmi_supported, message_tag;
  int retval = -1;

  assert(buffer != NULL && from != NULL);
  
  if (buflen == 0)
    return 0;

  obj_rmcp_hdr = _fiid_obj_create(tmpl_rmcp_hdr);
  obj_rmcp_cmd = _fiid_obj_create(tmpl_cmd_asf_presence_pong);

#ifndef NDEBUG
  if (debug)
    {
      if (ipmi_dump_rmcp_packet(STDERR_FILENO, 
                                "Pong", 
                                NULL,
                                (uint8_t *)buffer,
                                (uint32_t)buflen, 
                                tmpl_cmd_asf_presence_pong) < 0)
        ipmi_ping_err_exit("ipmi_dump_rmcp_packet: %s", strerror(errno));
    }
#endif

  if (unassemble_rmcp_pkt(buffer, buflen, obj_rmcp_hdr, obj_rmcp_cmd) < 0)
    ipmi_ping_err_exit("unassemble_rmcp_pkt: %s", strerror(errno));

  _fiid_obj_get(obj_rmcp_cmd, "message_type", (uint64_t *)&message_type);

  if (message_type != RMCP_ASF_MESSAGE_TYPE_PRESENCE_PONG)
    {
      retval = 0;
      goto cleanup;
    }

  _fiid_obj_get(obj_rmcp_cmd, "message_tag", (uint64_t *)&message_tag);
  if (message_tag != (sequence_number % (RMCP_ASF_MESSAGE_TAG_MAX + 1)))
    {
      retval = 0;
      goto cleanup;
    }

  printf("pong received from %s: message_tag=%u", from, (uint32_t)message_tag);
  if (verbose)
    {
      _fiid_obj_get(obj_rmcp_cmd, 
		    "supported_entities.ipmi_supported", 
		    (uint64_t *)&ipmi_supported);
      printf(", ipmi %s", _supported(ipmi_supported));
    }
  printf("\n");
  
  retval = 1;
 cleanup:
  _fiid_obj_destroy(obj_rmcp_hdr);
  _fiid_obj_destroy(obj_rmcp_cmd);
  return retval;
}

void 
latepacket(unsigned int sequence_number) 
{
  printf("pong timed out: message_tag=%u\n", sequence_number % (RMCP_ASF_MESSAGE_TAG_MAX + 1));
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
  ipmi_ping_setup(argc, argv, 0, RMCP_ASF_MESSAGE_TAG_MAX, "hVc:i:I:t:vs:d");
#else
  ipmi_ping_setup(argc, argv, 0, RMCP_ASF_MESSAGE_TAG_MAX, "hVc:i:I:t:vs:");
#endif
  ipmi_ping_loop(createpacket, parsepacket, latepacket, endresult);
  exit(1);                    /* NOT REACHED */
}
