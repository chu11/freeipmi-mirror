/*****************************************************************************\
 *  $Id: ipmipower_connection.c,v 1.54 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2012 Lawrence Livermore National Security, LLC.
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
#if HAVE_STRINGS_H
#include <strings.h>            /* bzero */
#endif /* HAVE_STRINGS_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>

#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include "ipmipower_connection.h"
#include "ipmipower_error.h"
#include "ipmipower_output.h"
#include "ipmipower_util.h"

#include "freeipmi-portability.h"
#include "cbuf.h"
#include "hostlist.h"

extern int h_errno;

extern cbuf_t ttyout;

extern struct ipmipower_arguments cmd_args;

#define IPMIPOWER_MIN_CONNECTION_BUF 1024*2
#define IPMIPOWER_MAX_CONNECTION_BUF 1024*4

/* _clean_fd
 * - Remove any extraneous packets sitting on the fd buf
 */
static void
_clean_fd (int fd)
{
  struct pollfd ufds;
  uint8_t buf[IPMIPOWER_PACKET_BUFLEN];

  while (1)
    {
      ssize_t rv;

      ufds.fd = fd;
      ufds.events = POLLIN;
      ufds.revents = 0;

      /* Must use poll, we may go outside of the fd numbers
       * that select is capable of using
       */

      ipmipower_poll (&ufds, 1, 0);

      if (ufds.revents & POLLIN)
        {
          rv = recvfrom (fd, buf, IPMIPOWER_PACKET_BUFLEN, 0, NULL, NULL);
          if (rv <= 0)
            break;
        }
      else
        break;

      IPMIPOWER_DEBUG (("removed packet: %d", rv));
    }
}

void
ipmipower_connection_clear (struct ipmipower_connection *ic)
{
  assert (ic);

  _clean_fd (ic->ipmi_fd);
  if (cbuf_drop (ic->ipmi_in, -1) < 0)
    {
      IPMIPOWER_ERROR (("cbuf_drop: %s", strerror (errno)));
      exit (1);
    }
  if (cbuf_drop (ic->ipmi_out, -1) < 0)
    {
      IPMIPOWER_ERROR (("cbuf_drop: %s", strerror (errno)));
      exit (1);
    }
  return;
}

static int
_connection_setup (struct ipmipower_connection *ic, const char *hostname)
{
  struct sockaddr_in srcaddr;
  struct hostent *result;

  assert (ic && hostname);

  /* Don't use wrapper function, need to exit cleanly on EMFILE errno */

  errno = 0;

  if ((ic->ipmi_fd = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
    {
      if (errno == EMFILE)
        {
          IPMIPOWER_DEBUG (("file descriptor limit reached"));
          return (-1);
        }
      
      IPMIPOWER_ERROR (("socket: %s", strerror (errno)));
      exit (1);
    }

  if ((ic->ping_fd = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
    {
      if (errno == EMFILE)
        {
          IPMIPOWER_DEBUG (("file descriptor limit reached"));
          return (-1);
        }

      IPMIPOWER_ERROR (("socket: %s", strerror (errno)));
      exit (1);
    }

  /* Secure ephemeral ports */
  bzero (&srcaddr, sizeof (struct sockaddr_in));
  srcaddr.sin_family = AF_INET;
  srcaddr.sin_port = htons (0);
  srcaddr.sin_addr.s_addr = htonl (INADDR_ANY);

  if (bind (ic->ipmi_fd, &srcaddr, sizeof (struct sockaddr_in)) < 0)
    {
      IPMIPOWER_ERROR (("bind: %s", strerror (errno)));
      exit (1);
    }
  if (bind (ic->ping_fd, &srcaddr, sizeof (struct sockaddr_in)) < 0)
    {
      IPMIPOWER_ERROR (("bind: %s", strerror (errno)));
      exit (1);
    }

  if (!(ic->ipmi_in  = cbuf_create (IPMIPOWER_MIN_CONNECTION_BUF,
                                    IPMIPOWER_MAX_CONNECTION_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      exit (1);
    }
  cbuf_opt_set (ic->ipmi_in, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);

  if (!(ic->ipmi_out = cbuf_create (IPMIPOWER_MIN_CONNECTION_BUF,
                                    IPMIPOWER_MAX_CONNECTION_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      exit (1);
    }
  cbuf_opt_set (ic->ipmi_out, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);

  if (!(ic->ping_in  = cbuf_create (IPMIPOWER_MIN_CONNECTION_BUF,
                                    IPMIPOWER_MAX_CONNECTION_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      exit (1);
    }
  cbuf_opt_set (ic->ping_in, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);

  if (!(ic->ping_out = cbuf_create (IPMIPOWER_MIN_CONNECTION_BUF,
                                    IPMIPOWER_MAX_CONNECTION_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      exit (1);
    }
  cbuf_opt_set (ic->ping_out, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);
  
  if (ipmi_get_random (&ic->ipmi_requester_sequence_number_counter,
                       sizeof (ic->ipmi_requester_sequence_number_counter)) < 0)
    {
      IPMIPOWER_ERROR (("ipmi_get_random: %s", strerror (errno)));
      exit (1);
    }
  
  if (ipmi_get_random (&ic->ping_sequence_number_counter,
                       sizeof (ic->ping_sequence_number_counter)) < 0)
    {
      IPMIPOWER_ERROR (("ipmi_get_random: %s", strerror (errno)));
      exit (1);
    }

  memset (&ic->last_ipmi_send, '\0', sizeof (struct timeval));
  memset (&ic->last_ping_send, '\0', sizeof (struct timeval));
  memset (&ic->last_ipmi_recv, '\0', sizeof (struct timeval));
  memset (&ic->last_ping_recv, '\0', sizeof (struct timeval));

  ic->link_state = LINK_GOOD; /* assumed good to begin with */
  ic->ping_last_packet_recv_flag = 0;
  ic->ping_packet_count_send = 0;
  ic->ping_packet_count_recv = 0;
  ic->ping_consec_count = 0;

  ic->discover_state = STATE_UNDISCOVERED;

  strncpy (ic->hostname, hostname, MAXHOSTNAMELEN);
  ic->hostname[MAXHOSTNAMELEN] = '\0';

  /* Determine the destination address */
  bzero (&(ic->destaddr), sizeof (struct sockaddr_in));
  ic->destaddr.sin_family = AF_INET;
  ic->destaddr.sin_port = htons (RMCP_PRIMARY_RMCP_PORT);

  if (!(result = gethostbyname (ic->hostname)))
    {
      if (h_errno == HOST_NOT_FOUND)
        ipmipower_output (MSG_TYPE_HOSTNAME_INVALID, ic->hostname);
      else
        {
#if HAVE_HSTRERROR
          IPMIPOWER_ERROR (("gethostbyname() %s: %s", ic->hostname, hstrerror (h_errno)));
#else /* !HAVE_HSTRERROR */
          IPMIPOWER_ERROR (("gethostbyname() %s: h_errno = %d", ic->hostname, h_errno));
#endif /* !HAVE_HSTRERROR */
          exit (1);
        }
      return (-1);
    }
  ic->destaddr.sin_addr = *((struct in_addr *)result->h_addr);

  ic->skip = 0;

  return (0);
}

struct ipmipower_connection *
ipmipower_connection_array_create (const char *hostname, unsigned int *len)
{
  char *str = NULL;
  int index = 0;
  hostlist_t hl = NULL;
  hostlist_iterator_t itr = NULL;
  struct ipmipower_connection *ics = NULL;
  int size = sizeof (struct ipmipower_connection);
  int hl_count;
  int errcount = 0;
  int emfilecount = 0;

  assert (hostname && len);

  *len = 0;

  if (!(hl = hostlist_create (hostname)))
    {
      ipmipower_output (MSG_TYPE_HOSTNAME_INVALID, hostname);
      return (NULL);
    }

  if (!(itr = hostlist_iterator_create (hl)))
    {
      IPMIPOWER_ERROR (("hostlist_iterator_create: %s", strerror (errno)));
      exit (1);
    }

  hostlist_uniq (hl);

  hl_count = hostlist_count (hl);

  if (!(ics = (struct ipmipower_connection *)malloc (size * hl_count)))
    {
      IPMIPOWER_ERROR (("malloc: %s", strerror (errno)));
      exit (1);
    }

  memset (ics, '\0', (size * hl_count));

  while ((str = hostlist_next (itr)))
    {
      ics[index].ipmi_fd = -1;
      ics[index].ping_fd = -1;

      /* cleanup only at the end, gather all error outputs for
       * later
       */
      if (_connection_setup (&ics[index], str) < 0)
        {
          if (errno == EMFILE && !emfilecount)
            {
              IPMIPOWER_DEBUG (("file descriptor limit reached"));
              /* XXX return -1? */
              emfilecount++;
            }
          errcount++;
        }

      free (str);
      index++;
    }

  hostlist_iterator_destroy (itr);
  hostlist_destroy (hl);

  if (errcount)
    {
      int i;
      for (i = 0; i < hl_count; i++)
        {
          /* ignore potential error, error path */
          close (ics[i].ipmi_fd);
          /* ignore potential error, error path */
          close (ics[i].ping_fd);
          if (ics[i].ipmi_in)
            cbuf_destroy (ics[i].ipmi_in);
          if (ics[i].ipmi_out)
            cbuf_destroy (ics[i].ipmi_out);
          if (ics[i].ping_in)
            cbuf_destroy (ics[i].ping_in);
          if (ics[i].ping_out)
            cbuf_destroy (ics[i].ping_out);
        }
      free (ics);
      return (NULL);
    }

  *len = hl_count;
  return (ics);
}

void
ipmipower_connection_array_destroy (struct ipmipower_connection *ics,
                                    unsigned int ics_len)
{
  int i;

  if (!ics)
    return;

  for (i = 0; i < ics_len; i++)
    {
      /* ignore potential error, cleanup path */
      close (ics[i].ipmi_fd);
      /* ignore potential error, cleanup path */
      close (ics[i].ping_fd);
      cbuf_destroy (ics[i].ipmi_in);
      cbuf_destroy (ics[i].ipmi_out);
      cbuf_destroy (ics[i].ping_in);
      cbuf_destroy (ics[i].ping_out);
    }
  free (ics);
}

int
ipmipower_connection_hostname_index (struct ipmipower_connection *ics,
                                     unsigned int ics_len,
                                     const char *hostname)
{
  int i;

  assert (ics && ics_len && hostname);

  for (i = 0; i < ics_len; i++)
    {
      if (!strcmp (ics[i].hostname, hostname))
        return (i);
    }

  IPMIPOWER_DEBUG (("host = %s not found", hostname));
  return (-1);
}
