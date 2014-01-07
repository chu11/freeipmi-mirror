/*****************************************************************************\
 *  $Id: rmcpping.c,v 1.55 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155448
 *
 *  This file is part of Ipmiping, tools for pinging IPMI and RMCP compliant
 *  remote systems. For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiping is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiping is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiping.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

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

#include "freeipmi-portability.h"
#include "debug-util.h"
#include "ping-tool-common.h"

#define _supported(x)   (x) ? "supported" : "not-supported"

int
createpacket (const char *destination,
              void *buf,
              unsigned int buflen,
              unsigned int sequence_number,
              int version,
              int debug)
{
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_rmcp_cmd = NULL;
  int len;

  assert (destination);
  assert (buf);

  if (!buflen)
    return (0);

  if (!(obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));
  if (!(obj_rmcp_cmd = fiid_obj_create (tmpl_cmd_asf_presence_ping)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));

  if (fill_rmcp_hdr_asf (obj_rmcp_hdr) < 0)
    ipmi_ping_err_exit ("fill_rmcp_hdr_asf: %s", strerror (errno));

  /* Avoid use of message tag number 0xFF.  Behavior of message tag 0xFF
   * is unpredictable.  See IPMI 1.5 Specification and DMTF RMCP
   * specification for details.
   */

  if (fill_cmd_asf_presence_ping (sequence_number % (RMCP_ASF_MESSAGE_TAG_MAX + 1),
                                  obj_rmcp_cmd) < 0)
    ipmi_ping_err_exit ("fill_cmd_asf_presence_ping: %s", strerror (errno));

  if ((len = assemble_rmcp_pkt (obj_rmcp_hdr,
                                obj_rmcp_cmd,
                                buf,
                                buflen,
				IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    ipmi_ping_err_exit ("assemble_rmcp_pkt: %s", strerror (errno));

  if (debug)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
                     DEBUG_UTIL_DIRECTION_NONE,
		     DEBUG_UTIL_FLAGS_DEFAULT,
                     DEBUG_UTIL_RMCPPING_STR,
                     hdrbuf,
                     DEBUG_UTIL_HDR_BUFLEN);

      if (ipmi_dump_rmcp_packet (STDERR_FILENO,
                                 destination,
                                 hdrbuf,
                                 NULL,
                                 buf,
                                 len,
                                 tmpl_cmd_asf_presence_ping) < 0)
        ipmi_ping_err_exit ("ipmi_dump_rmcp_packet: %s", strerror (errno));
    }

  fiid_obj_destroy (obj_rmcp_hdr);
  fiid_obj_destroy (obj_rmcp_cmd);
  return (len);
}

int
parsepacket (const char *destination,
             const void *buf,
             unsigned int buflen,
             const char *from,
             unsigned int sequence_number,
             int verbose,
             int version,
             int debug)
{
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_rmcp_cmd = NULL;
  uint8_t message_type, message_tag;
  uint64_t val;
  int ret, rv = -1;

  assert (destination);
  assert (buf);
  assert (from);

  if (!buflen)
    return (0);

  if (!(obj_rmcp_hdr = fiid_obj_create (tmpl_rmcp_hdr)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));
  if (!(obj_rmcp_cmd = fiid_obj_create (tmpl_cmd_asf_presence_pong)))
    ipmi_ping_err_exit ("fiid_obj_create: %s", strerror (errno));

  if (debug)
    {
      char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];

      debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
                     DEBUG_UTIL_DIRECTION_NONE,
		     DEBUG_UTIL_FLAGS_DEFAULT,
                     DEBUG_UTIL_RMCPPONG_STR,
                     hdrbuf,
                     DEBUG_UTIL_HDR_BUFLEN);

      if (ipmi_dump_rmcp_packet (STDERR_FILENO,
                                 destination,
                                 hdrbuf,
                                 NULL,
                                 buf,
                                 buflen,
                                 tmpl_cmd_asf_presence_pong) < 0)
        ipmi_ping_err_exit ("ipmi_dump_rmcp_packet: %s", strerror (errno));
    }

  if ((ret = unassemble_rmcp_pkt (buf,
                                  buflen,
                                  obj_rmcp_hdr,
                                  obj_rmcp_cmd,
				  IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    ipmi_ping_err_exit ("unassemble_rmcp_pkt: %s", strerror (errno));

  if (!ret)
    {
      rv = 0;
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_rmcp_cmd,
                    "message_type",
                    &val) < 0)
    ipmi_ping_err_exit ("fiid_obj_get: 'message_type': %s",
                        fiid_obj_errormsg (obj_rmcp_cmd));
  message_type = val;
  
  if (message_type != RMCP_ASF_MESSAGE_TYPE_PRESENCE_PONG)
    {
      rv = 0;
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_rmcp_cmd,
                    "message_tag",
                    &val) < 0)
    ipmi_ping_err_exit ("fiid_obj_get: 'message_tag': %s",
                        fiid_obj_errormsg (obj_rmcp_cmd));
  message_tag = val;
  
  if (message_tag != (sequence_number % (RMCP_ASF_MESSAGE_TAG_MAX + 1)))
    {
      rv = 0;
      goto cleanup;
    }

  printf ("pong received from %s: message_tag=%u", from, message_tag);
  if (verbose)
    {
      uint8_t ipmi_supported;
      
      if (FIID_OBJ_GET (obj_rmcp_cmd,
                        "supported_entities.ipmi_supported",
                        &val) < 0)
        ipmi_ping_err_exit ("fiid_obj_get: 'supported_entities.ipmi_supported': %s",
                            fiid_obj_errormsg (obj_rmcp_cmd));
      ipmi_supported = val;
      
      printf (", ipmi %s", _supported (ipmi_supported));
    }
  printf ("\n");

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_rmcp_hdr);
  fiid_obj_destroy (obj_rmcp_cmd);
  return (rv);
}

void
latepacket (unsigned int sequence_number)
{
  printf ("pong timed out: message_tag=%u\n", sequence_number % (RMCP_ASF_MESSAGE_TAG_MAX + 1));
}

int
endresult (const char *progname,
           const char *destination,
           unsigned int sent_count,
           unsigned int recv_count)
{
  double percent = 0;

  assert (progname);
  assert (destination);

  if (sent_count > 0)
    percent = ((double)(sent_count - recv_count)/sent_count)*100;

  printf ("--- %s %s statistics ---\n", progname, destination);
  printf ("%d pings transmitted, %d pongs received in time, "
          "%2.1f%% packet loss\n",
          sent_count, recv_count, percent);

  return ((recv_count > 0) ? 0 : 1);
}

int
main (int argc, char **argv)
{
  ipmi_ping_setup (argc, argv, 0, RMCP_ASF_MESSAGE_TAG_MAX, "hVc:i:I:t:vs:d");
  ipmi_ping_loop (createpacket, parsepacket, latepacket, endresult);
  exit (EXIT_FAILURE);                    /* NOT REACHED */
}
