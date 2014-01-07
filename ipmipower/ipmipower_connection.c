/*****************************************************************************\
 *  $Id: ipmipower_connection.c,v 1.54 2010-02-08 22:02:31 chu11 Exp $
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
  struct sockaddr_in srcaddr;
  struct hostent *result;
  char *hostname_first_parse_copy = NULL;
  const char *hostname_first_parse_ptr = NULL;
  char *hostname_second_parse_copy = NULL;
  const char *hostname_second_parse_ptr = NULL;
  uint16_t port = RMCP_PRIMARY_RMCP_PORT;
  int rv = -1;

  assert (ic);
  assert (hostname);

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
      exit (EXIT_FAILURE);
    }

  if ((ic->ping_fd = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
    {
      if (errno == EMFILE)
        {
          IPMIPOWER_DEBUG (("file descriptor limit reached"));
          return (-1);
        }

      IPMIPOWER_ERROR (("socket: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  /* Secure ephemeral ports */
  bzero (&srcaddr, sizeof (struct sockaddr_in));
  srcaddr.sin_family = AF_INET;
  srcaddr.sin_port = htons (0);
  srcaddr.sin_addr.s_addr = htonl (INADDR_ANY);

  if (bind (ic->ipmi_fd, &srcaddr, sizeof (struct sockaddr_in)) < 0)
    {
      IPMIPOWER_ERROR (("bind: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (bind (ic->ping_fd, &srcaddr, sizeof (struct sockaddr_in)) < 0)
    {
      IPMIPOWER_ERROR (("bind: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

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

  if (strchr (hostname_first_parse_ptr, ':'))
    {
      char *ptr;

      if (!(hostname_second_parse_copy = strdup (hostname_first_parse_ptr)))
	{
	  IPMIPOWER_ERROR (("strdup: %s", strerror (errno)));
	  exit (EXIT_FAILURE);
	}

      if ((ptr = strchr (hostname_second_parse_copy, ':')))
	{
	  char *endptr;
          int tmp;

	  *ptr = '\0';
          ptr++;
	  
	  hostname_second_parse_ptr = hostname_second_parse_copy;

          errno = 0;
          tmp = strtol (ptr, &endptr, 0);
          if (errno
              || endptr[0] != '\0'
              || tmp <= 0
              || tmp > USHRT_MAX)
            {
	      ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, hostname_second_parse_ptr, NULL);
	      goto cleanup;
            }
	  
          port = tmp;
	}
      else
	hostname_second_parse_ptr = hostname_second_parse_copy;
    }
  else
    hostname_second_parse_ptr = hostname_first_parse_ptr;

  strncpy (ic->hostname, hostname_second_parse_ptr, MAXHOSTNAMELEN);
  ic->hostname[MAXHOSTNAMELEN] = '\0';

  /* Determine the destination address */
  bzero (&(ic->destaddr), sizeof (struct sockaddr_in));
  ic->destaddr.sin_family = AF_INET;
  ic->destaddr.sin_port = htons (port);

  if (!(result = gethostbyname (ic->hostname)))
    {
      if (h_errno == HOST_NOT_FOUND)
        ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, ic->hostname, NULL);
      else
        {
#if HAVE_HSTRERROR
          IPMIPOWER_ERROR (("gethostbyname() %s: %s", ic->hostname, hstrerror (h_errno)));
#else /* !HAVE_HSTRERROR */
          IPMIPOWER_ERROR (("gethostbyname() %s: h_errno = %d", ic->hostname, h_errno));
#endif /* !HAVE_HSTRERROR */
          exit (EXIT_FAILURE);
        }
      goto cleanup;
    }
  ic->destaddr.sin_addr = *((struct in_addr *)result->h_addr);

  ic->skip = 0;

  rv = 0;
 cleanup:
  free (hostname_first_parse_copy);
  free (hostname_second_parse_copy);
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
      hostlist_t h = NULL;

      /* if invalid to hostlist, it still may be valid in general, so fall through */
      if (!(h = hostlist_create (extra_arg)))
	goto one_extra_arg;

      if (hostlist_count (h) > 1)
	{
	  hostlist_iterator_t hitr = NULL;
	  char *extrastr;

	  if (!(hitr = hostlist_iterator_create (h)))
	    {
	      IPMIPOWER_ERROR (("hostlist_iterator_create: %s", strerror (errno)));
	      exit (EXIT_FAILURE);
	    }
	  
	  while ((extrastr = hostlist_next (hitr)))
	    {
	      _connection_add_extra_arg_base (ic, extrastr);
	      free (extrastr);
	    }
	  
	  hostlist_iterator_destroy (hitr);
	  hostlist_destroy (h);
	  return;
	}
      
      hostlist_destroy (h);
      goto one_extra_arg;
    }

 one_extra_arg:
  _connection_add_extra_arg_base (ic, extra_arg);
}

int
_hostname_count (const char *hostname)
{
  hostlist_t h = NULL;
  hostlist_t h2 = NULL;
  hostlist_iterator_t hitr = NULL;
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
   * foohost[1-3]+[1-3] is parsed with the hostlist library, it can be
   * parsed as foohost[1-3]+1, foohost[1-3]+2, foohost[1-3]+3.  So it
   * can appear like 9 hosts when in fact it is 3.
   *
   * This function will attempt to handle everthing and count up
   * everything appropriately.
   */

  if (!(h = hostlist_create (hostname)))
    {
      ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, hostname, NULL);
      goto cleanup;
    }

  if (!(h2 = hostlist_create (NULL)))
    {
      IPMIPOWER_ERROR (("hostlist_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  hostlist_uniq (h);

  if (!(hitr = hostlist_iterator_create (h)))
    {
      IPMIPOWER_ERROR (("hostlist_iterator_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
      
  while ((hstr = hostlist_next (hitr)))
    {
      char *ptr;
      
      if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
	{
	  if ((ptr = strchr (hstr, '+')))
	    *ptr = '\0';
	}

      if (!hostlist_push (h2, hstr))
	{
	  IPMIPOWER_ERROR (("hostlist_push: %s", strerror(errno)));
	  exit (EXIT_FAILURE);
	}
      
      free (hstr);
    }

  hostlist_uniq (h2);
  
  rv = hostlist_count (h2);

 cleanup:
  hostlist_iterator_destroy (hitr);
  hostlist_destroy (h);
  hostlist_destroy (h2);
  return (rv);
}

struct ipmipower_connection *
ipmipower_connection_array_create (const char *hostname, unsigned int *len)
{
  int index = 0;
  hostlist_t h = NULL;
  hostlist_iterator_t hitr = NULL;
  hostlist_t h2 = NULL;
  hostlist_iterator_t h2itr = NULL;
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
 
  if (!(h = hostlist_create (hostname)))
    {
      ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, hostname, NULL);
      errflag++;
      goto cleanup;
    }

  hostlist_uniq (h);

  if (!(hitr = hostlist_iterator_create (h)))
    {
      IPMIPOWER_ERROR (("hostlist_iterator_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  while ((hstr = hostlist_next (hitr)))
    {
      /* achu: The double hostlist_create is to handle the corner case
       * of someone inputting.
       *
       * foohost[1-3]+[1-3]
       *
       * We need to double hostlist to get all the hosts and extra
       * args.
       *
       * Under most scenarios, this is just inefficient code.
       * However, this is normally a one time setup cost, so shouldn't
       * affect the overall running of ipmipower.  In addition, the
       * code logic is simpler to do it this way then have a whole
       * bunch of wacky if-check scenarios to make it more efficient.
       */

      if (!(h2 = hostlist_create (hstr)))
	{
	  ipmipower_output (IPMIPOWER_MSG_TYPE_HOSTNAME_INVALID, hostname, NULL);
	  errflag++;
	  goto cleanup;
	}
      
      hostlist_uniq (h2);

      if (!(h2itr = hostlist_iterator_create (h2)))
	{
	  IPMIPOWER_ERROR (("hostlist_iterator_create: %s", strerror (errno)));
	  exit (EXIT_FAILURE);
	}

      while ((h2str = hostlist_next (h2itr)))
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

      hostlist_iterator_destroy (h2itr);
      hostlist_destroy (h2);
      h2itr = NULL;
      h2 = NULL;
      free (hstr);
      hstr = NULL;
    }

 cleanup:
  hostlist_iterator_destroy (h2itr);
  hostlist_destroy (h2);
  hostlist_iterator_destroy (hitr);
  hostlist_destroy (h);
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
