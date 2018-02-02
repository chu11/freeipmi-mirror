/*****************************************************************************\
 *  $Id: ipmipower_connection.c,v 1.54 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2015 Lawrence Livermore National Security, LLC.
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
#include <limits.h>
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
#include "fi_hostlist.h"
#include "network.h"

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
      exit (EXIT_FAILURE);
    }
  if (cbuf_drop (ic->ipmi_out, -1) < 0)
    {
      IPMIPOWER_ERROR (("cbuf_drop: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  return;
}

static int
_connection_setup (struct ipmipower_connection *ic, const char *hostname)
{
  char *hostname_first_parse_copy = NULL;
  const char *hostname_first_parse_ptr = NULL;
  char *hostname_second_parse_copy = NULL;
  char *port_second_parse_copy = NULL;
  const char *hostname_second_parse_ptr = NULL;
  uint16_t port = RMCP_PRIMARY_RMCP_PORT;
  const char *port_ptr = NULL;
  char port_str[MAXPORTBUFLEN + 1];
  struct addrinfo ai_hints, *ai_res = NULL, *ai = NULL;
  int rv = -1;
  int ret;

  assert (ic);
  assert (hostname);

  /* Don't use wrapper function, need to exit cleanly on EMFILE errno */

  errno = 0;

  if (!(ic->ipmi_in  = cbuf_create (IPMIPOWER_MIN_CONNECTION_BUF,
                                    IPMIPOWER_MAX_CONNECTION_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  cbuf_opt_set (ic->ipmi_in, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);

  if (!(ic->ipmi_out = cbuf_create (IPMIPOWER_MIN_CONNECTION_BUF,
                                    IPMIPOWER_MAX_CONNECTION_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  cbuf_opt_set (ic->ipmi_out, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);

  if (!(ic->ping_in  = cbuf_create (IPMIPOWER_MIN_CONNECTION_BUF,
                                    IPMIPOWER_MAX_CONNECTION_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  cbuf_opt_set (ic->ping_in, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);

  if (!(ic->ping_out = cbuf_create (IPMIPOWER_MIN_CONNECTION_BUF,
                                    IPMIPOWER_MAX_CONNECTION_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  cbuf_opt_set (ic->ping_out, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);

  if (ipmi_get_random (&ic->ipmi_requester_sequence_number_counter,
                       sizeof (ic->ipmi_requester_sequence_number_counter)) < 0)
    {
      IPMIPOWER_ERROR (("ipmi_get_random: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (ipmi_get_random (&ic->ping_sequence_number_counter,
                       sizeof (ic->ping_sequence_number_counter)) < 0)
    {
      IPMIPOWER_ERROR (("ipmi_get_random: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  memset (&ic->last_ipmi_send, '\0', sizeof (struct timeval));
  memset (&ic->last_ping_send, '\0', sizeof (struct timeval));
  memset (&ic->last_ipmi_recv, '\0', sizeof (struct timeval));
  memset (&ic->last_ping_recv, '\0', sizeof (struct timeval));

  ic->link_state = IPMIPOWER_LINK_STATE_GOOD; /* assumed good to begin with */
  ic->ping_last_packet_recv_flag = 0;
  ic->ping_packet_count_send = 0;
  ic->ping_packet_count_recv = 0;
  ic->ping_consec_count = 0;

  ic->discover_state = IPMIPOWER_DISCOVER_STATE_UNDISCOVERED;

  if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
    {
      struct ipmipower_connection_extra_arg *ea;
      char *extra_arg = NULL;
      char *ptr;

      if (!(hostname_first_parse_copy = strdup (hostname)))
        {
          IPMIPOWER_ERROR (("strdup: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }

      if ((ptr = strchr (hostname_first_parse_copy, '+')))
        {
          *ptr = '\0';
          ptr++;
          extra_arg = ptr;
          hostname_first_parse_ptr = hostname_first_parse_copy;
        }
      else
        hostname_first_parse_ptr = hostname;

      /* Hypothetically, some OEM power types may allow extra-args and
       * not extra args.  So store both.
       */

      if (!(ea = (struct ipmipower_connection_extra_arg *)malloc (sizeof (struct ipmipower_connection_extra_arg))))
        {
          IPMIPOWER_ERROR (("malloc: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }

      if (extra_arg)
        {
          if (!(ea->extra_arg = strdup (extra_arg)))
            {
              IPMIPOWER_ERROR (("strdup: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }
        }
      else
        ea->extra_arg = NULL;
      ea->next = NULL;

      ic->extra_args = ea;
    }
  else
    hostname_first_parse_ptr = hostname;

  /* Check for host:port or [Ipv6]:port format */
  if ((ret = host_is_host_with_port (hostname_first_parse_ptr,
                                     &hostname_second_parse_copy,
                                     &port_second_parse_copy)) < 0)
    {
      IPMIPOWER_ERROR (("host_is_host_with_port: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (ret)
    {
      hostname_second_parse_ptr = hostname_second_parse_copy;
      port_ptr = port_second_parse_copy;
    }
  else
    hostname_second_parse_ptr = hostname_first_parse_ptr;

  if ((ret = host_is_valid (hostname_second_parse_ptr,
                            port_ptr,
                            &port)) < 0)
    {
      IPMIPOWER_ERROR (("host_is_valid: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (!ret)
    {
      ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, hostname_second_parse_ptr, NULL);
      goto cleanup;
    }

  strncpy (ic->hostname, hostname_second_parse_ptr, MAXHOSTNAMELEN);
  ic->hostname[MAXHOSTNAMELEN] = '\0';

  memset (port_str, '\0', MAXPORTBUFLEN + 1);
  snprintf (port_str, MAXPORTBUFLEN, "%d", port);
  memset (&ai_hints, 0, sizeof (struct addrinfo));
  ai_hints.ai_family = AF_UNSPEC;
  ai_hints.ai_socktype = SOCK_DGRAM;
  ai_hints.ai_flags = (AI_V4MAPPED | AI_ADDRCONFIG);

  if ((ret = getaddrinfo (ic->hostname, port_str, &ai_hints, &ai_res)))
    {
      if (ret == EAI_NODATA
	  || ret == EAI_NONAME)
        ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, ic->hostname, NULL);
      else
        {
          IPMIPOWER_ERROR (("getaddrinfo() %s: %s", ic->hostname, gai_strerror (ret)));
          exit (EXIT_FAILURE);
        }
      goto cleanup;
    }

  /* Try all of the different answers we got, until we succeed. */
  for (ai = ai_res; ai != NULL; ai = ai->ai_next)
    {
      if ((ic->ipmi_fd = socket (ai->ai_family,
				 ai->ai_socktype, ai->ai_protocol)) < 0)
	{
	  if (errno == EMFILE)
	    {
	      IPMIPOWER_DEBUG (("file descriptor limit reached"));
	      return (-1);
	    }
	}

      if ((ic->ping_fd = socket (ai->ai_family,
				 ai->ai_socktype, ai->ai_protocol)) < 0)
	{
	  if (errno == EMFILE)
	    {
	      IPMIPOWER_DEBUG (("file descriptor limit reached"));
	      return (-1);
	    }
	}

      if (ai->ai_family == AF_INET)
        {
          memcpy (&(ic->destaddr4), ai->ai_addr, ai->ai_addrlen);
          ic->destaddr = (struct sockaddr *)&(ic->destaddr4);
          ic->destaddrlen = sizeof (struct sockaddr_in);

          /* zero everywhere, secure ephemeral port */
          memset (&(ic->srcaddr4), '\0', sizeof (struct sockaddr_in));
          ic->srcaddr4.sin_family = AF_INET;

          ic->srcaddr = (struct sockaddr *)&(ic->srcaddr4);
          ic->srcaddrlen = sizeof (struct sockaddr_in);
        }
      else if (ai->ai_family == AF_INET6)
        {
          memcpy (&(ic->destaddr6), ai->ai_addr, ai->ai_addrlen);
          ic->destaddr = (struct sockaddr *)&(ic->destaddr6);
          ic->destaddrlen = sizeof (struct sockaddr_in6);

          /* zero everywhere, secure ephemeral port */
          memset (&(ic->srcaddr6), '\0', sizeof (struct sockaddr_in6));
          ic->srcaddr6.sin6_family = AF_INET6;
          ic->srcaddr = (struct sockaddr *)&(ic->srcaddr6);
          ic->srcaddrlen = sizeof (struct sockaddr_in6);
        }
      else
        {
	  close(ic->ipmi_fd);
	  close(ic->ping_fd);
	  continue;
        }

      if ((bind (ic->ipmi_fd, ic->srcaddr, ic->srcaddrlen) < 0)
          || (bind (ic->ping_fd, ic->srcaddr, ic->srcaddrlen) < 0))
	{
	  close(ic->ipmi_fd);
	  close(ic->ping_fd);
	  continue;
	}

      ic->skip = 0;
      break;
    }

  if (!ai)
    {
      ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, hostname_second_parse_ptr, NULL);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  freeaddrinfo (ai_res);
  free (hostname_first_parse_copy);
  free (hostname_second_parse_copy);
  free (port_second_parse_copy);
  return (rv);
}

static void
_connection_add_extra_arg_base (struct ipmipower_connection *ic, const char *extra_arg)
{
  struct ipmipower_connection_extra_arg *ea;
  struct ipmipower_connection_extra_arg *eanode;

  assert (ic);
  assert (ic->extra_args);
  assert (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE);

  eanode = ic->extra_args;
  while (eanode)
    {
      /* if duplicate, just return, no need to store */

      if (!eanode->extra_arg && !extra_arg)
        return;
      if (eanode->extra_arg
          && extra_arg
          && !strcmp (eanode->extra_arg, extra_arg))
        return;
      eanode = eanode->next;
    }

  if (!(ea = (struct ipmipower_connection_extra_arg *)malloc (sizeof (struct ipmipower_connection_extra_arg))))
    {
      IPMIPOWER_ERROR (("malloc: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (extra_arg)
    {
      if (!(ea->extra_arg = strdup (extra_arg)))
        {
          IPMIPOWER_ERROR (("strdup: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }
    }
  else
    ea->extra_arg = NULL;
  ea->next = NULL;

  eanode = ic->extra_args;
  while (eanode->next)
    eanode = eanode->next;
  eanode->next = ea;
}

static void
_connection_add_extra_arg (struct ipmipower_connection *ic, const char *extra_arg)
{
  assert (ic);
  assert (ic->extra_args);
  assert (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE);

  /* Some OEM power types could have ranges for the extra args */
  if (cmd_args.oem_power_type == IPMIPOWER_OEM_POWER_TYPE_C410X
      && extra_arg)
    {
      fi_hostlist_t h = NULL;

      /* if invalid to fi_hostlist, it still may be valid in general, so fall through */
      if (!(h = fi_hostlist_create (extra_arg)))
        goto one_extra_arg;

      if (fi_hostlist_count (h) > 1)
        {
          fi_hostlist_iterator_t hitr = NULL;
          char *extrastr;

          if (!(hitr = fi_hostlist_iterator_create (h)))
            {
              IPMIPOWER_ERROR (("fi_hostlist_iterator_create: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }

          while ((extrastr = fi_hostlist_next (hitr)))
            {
              _connection_add_extra_arg_base (ic, extrastr);
              free (extrastr);
            }

          fi_hostlist_iterator_destroy (hitr);
          fi_hostlist_destroy (h);
          return;
        }

      fi_hostlist_destroy (h);
      goto one_extra_arg;
    }

 one_extra_arg:
  _connection_add_extra_arg_base (ic, extra_arg);
}

int
_hostname_count (const char *hostname)
{
  fi_hostlist_t h = NULL;
  fi_hostlist_t h2 = NULL;
  fi_hostlist_iterator_t hitr = NULL;
  char *hstr = NULL;
  int rv = -1;

  /* achu:
   *
   * Possible user inputs are wide given extra-argument possibilities.
   *
   * foohost+1,foohost+2 is one host
   * foohost+[1-3] is one host
   * foohost[1-3]+A is 3 hosts
   * foohost[1-3]+[1-3] is 3 hosts
   *
   * The issue is that makes this complicated is when something like
   * foohost[1-3]+[1-3] is parsed with the fi_hostlist library, it can be
   * parsed as foohost[1-3]+1, foohost[1-3]+2, foohost[1-3]+3.  So it
   * can appear like 9 hosts when in fact it is 3.
   *
   * This function will attempt to handle everthing and count up
   * everything appropriately.
   */

  if (!(h = fi_hostlist_create (hostname)))
    {
      ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, hostname, NULL);
      goto cleanup;
    }

  if (!(h2 = fi_hostlist_create (NULL)))
    {
      IPMIPOWER_ERROR (("fi_hostlist_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  fi_hostlist_uniq (h);

  if (!(hitr = fi_hostlist_iterator_create (h)))
    {
      IPMIPOWER_ERROR (("fi_hostlist_iterator_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  while ((hstr = fi_hostlist_next (hitr)))
    {
      char *ptr;

      if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
        {
          if ((ptr = strchr (hstr, '+')))
            *ptr = '\0';
        }

      if (!fi_hostlist_push (h2, hstr))
        {
          IPMIPOWER_ERROR (("fi_hostlist_push: %s", strerror(errno)));
          exit (EXIT_FAILURE);
        }

      free (hstr);
    }

  fi_hostlist_uniq (h2);

  rv = fi_hostlist_count (h2);

 cleanup:
  fi_hostlist_iterator_destroy (hitr);
  fi_hostlist_destroy (h);
  fi_hostlist_destroy (h2);
  return (rv);
}

struct ipmipower_connection *
ipmipower_connection_array_create (const char *hostname, unsigned int *len)
{
  int index = 0;
  fi_hostlist_t h = NULL;
  fi_hostlist_iterator_t hitr = NULL;
  fi_hostlist_t h2 = NULL;
  fi_hostlist_iterator_t h2itr = NULL;
  char *hstr = NULL;
  char *h2str = NULL;
  struct ipmipower_connection *ics = NULL;
  int host_count;
  int errflag = 0;
  int emfilecount = 0;
  int i;

  assert (hostname && len);

  *len = 0;

  if ((host_count = _hostname_count (hostname)) < 0)
    return (NULL);

  if (!(ics = (struct ipmipower_connection *)malloc (sizeof (struct ipmipower_connection) * host_count)))
    {
      IPMIPOWER_ERROR (("malloc: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  memset (ics, '\0', (sizeof (struct ipmipower_connection) * host_count));

  for (i = 0; i < host_count; i++)
    {
      ics[i].ipmi_fd = -1;
      ics[i].ping_fd = -1;
    }

  if (!(h = fi_hostlist_create (hostname)))
    {
      ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, hostname, NULL);
      errflag++;
      goto cleanup;
    }

  fi_hostlist_uniq (h);

  if (!(hitr = fi_hostlist_iterator_create (h)))
    {
      IPMIPOWER_ERROR (("fi_hostlist_iterator_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  while ((hstr = fi_hostlist_next (hitr)))
    {
      /* achu: The double fi_hostlist_create is to handle the corner case
       * of someone inputting.
       *
       * foohost[1-3]+[1-3]
       *
       * We need to double fi_hostlist to get all the hosts and extra
       * args.
       *
       * Under most scenarios, this is just inefficient code.
       * However, this is normally a one time setup cost, so shouldn't
       * affect the overall running of ipmipower.  In addition, the
       * code logic is simpler to do it this way then have a whole
       * bunch of wacky if-check scenarios to make it more efficient.
       */

      if (!(h2 = fi_hostlist_create (hstr)))
        {
          ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, hostname, NULL);
          errflag++;
          goto cleanup;
        }

      fi_hostlist_uniq (h2);

      if (!(h2itr = fi_hostlist_iterator_create (h2)))
        {
          IPMIPOWER_ERROR (("fi_hostlist_iterator_create: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }

      while ((h2str = fi_hostlist_next (h2itr)))
        {
          /* We need to see if the host has already been saved to the
           * ics array.  It's possible under many circumstances with
           * extra args, such as
           *
           * foohost+1,foohost+2
           *
           * that it's already in the list.
           */

          if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
            {
              char *h2str_copy;
              char *ptr;
              int found = 0;

              if (!(h2str_copy = strdup (h2str)))
                {
                  IPMIPOWER_ERROR (("strdup: %s", strerror(errno)));
                  exit (EXIT_FAILURE);
                }

              if ((ptr = strchr (h2str_copy, '+')))
                {
                  *ptr = '\0';
                  ptr++;

                  /* XXX: This is O(n^2) slow.  99% of the time it's a one
                   * time setup cost, so we consider the slowness ok.  If
                   * it becomes a problem later, we'll need to
                   * rearchitect.
                   */

                  for (i = 0; i < index; i++)
                    {
                      if (!strcmp (ics[i].hostname, h2str_copy))
                        {
                          found++;

                          _connection_add_extra_arg (&ics[i], ptr);

                          break;
                        }
                    }
                }

              free (h2str_copy);

              if (found)
                {
                  free (h2str);
                  continue;
                }
            }

          if (index >= host_count)
            {
              IPMIPOWER_ERROR (("Invalid host count: %d", host_count));
              exit (EXIT_FAILURE);
            }

          /* cleanup only at the end, gather all error outputs for
           * later
           */
          if (_connection_setup (&ics[index], h2str) < 0)
            {
              if (errno == EMFILE && !emfilecount)
                {
                  IPMIPOWER_DEBUG (("file descriptor limit reached"));
                  /* XXX return -1? */
                  emfilecount++;
                }
              errflag++;
            }

          free (h2str);
          h2str = NULL;
          index++;
        }

      fi_hostlist_iterator_destroy (h2itr);
      fi_hostlist_destroy (h2);
      h2itr = NULL;
      h2 = NULL;
      free (hstr);
      hstr = NULL;
    }

 cleanup:
  fi_hostlist_iterator_destroy (h2itr);
  fi_hostlist_destroy (h2);
  fi_hostlist_iterator_destroy (hitr);
  fi_hostlist_destroy (h);
  free (h2str);
  free (hstr);

  if (errflag)
    {
      int i;
      for (i = 0; i < index; i++)
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
          if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
            {
              struct ipmipower_connection_extra_arg *eanode;
              assert (ics[i].extra_args);

              eanode = ics[i].extra_args;
              while (eanode)
                {
                  struct ipmipower_connection_extra_arg *eatmp;
                  eatmp = eanode->next;

                  free (eanode->extra_arg);
                  free (eanode);

                  eanode = eatmp;
                }
            }
        }
      free (ics);
      return (NULL);
    }

  *len = index;
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
      if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
        {
          struct ipmipower_connection_extra_arg *eanode;
          assert (ics[i].extra_args);

          eanode = ics[i].extra_args;
          while (eanode)
            {
              struct ipmipower_connection_extra_arg *eatmp;
              eatmp = eanode->next;

              free (eanode->extra_arg);
              free (eanode);

              eanode = eatmp;
            }
        }
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
