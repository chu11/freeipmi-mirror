/*****************************************************************************\
 *  $Id: hostlist.c,v 1.3 2009-12-16 17:49:39 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2015 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see https://savannah.gnu.org/projects/freeipmi/.
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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif  /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <ctype.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <limits.h>             /* MAXHOSTNAMELEN */
#ifdef HAVE_NETDB_H
#include <netdb.h>              /* MAXHOSTNAMELEN Solaris */
#endif /* HAVE_NETDB_H */

#include "fi_hostlist.h"
#include "hostlist.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

int
host_is_ipv6_with_port (const char *host, char **addr, char **port)
{
  char *str = NULL;
  int is_ipv6_with_port = 0;
  int rv = -1;

  assert (host);

  if (!(str = strdup (host)))
    goto cleanup;

  /* achu:
   *
   * Some of this code is from
   *
   * LaMont Jones <lamont@mmjgroup.com>
   *
   * In his experimental patch
   *
   * git pull https://github.com/lamontj/freeipmi-mirror.git ipmipower-ipv6
   *
   */

  if (str[0] == '[')
    {
      char *addrptr = &str[1];
      char *tmp;

      if ((tmp = strchr (addrptr, ']')))
        {
          *tmp = '\0';
          tmp++;

          /* Is character after the right bracket a colon? */
          if (*tmp == ':')
            {
              char *portptr;

              /* is everything after the colon a number? */
              tmp++;
              portptr = tmp;
              while (isdigit (*tmp))
                tmp++;

              /* are we at the end of the string? */
              /* and did we find a port */
              if (*tmp == '\0' && strlen (portptr))
                {
                  struct sockaddr_in6 saddr;

                  /* is what's in between the brackets an IPv6 address? */
                  if (inet_pton(AF_INET6, addrptr, &saddr) == 1)
                    {
                      /* if yes, we've got the special IPv6/port combo */
                      is_ipv6_with_port = 1;

                      if (addr)
                        {
                          if (!(*addr = strdup (addrptr)))
                            goto cleanup;
                        }

                      if (port)
                        {
                          if (!(*port = strdup (portptr)))
                            {
                              free (*addr);
                              goto cleanup;
                            }
                        }
                    }
                }
            }
        }
    }

  rv = is_ipv6_with_port;
 cleanup:
  free (str);
  return (rv);
}

int
host_is_host_with_port (const char *host, char **addr, char **port)
{
  char *str = NULL;
  int rv = -1;
  int is_host_with_port = 0;
  int ret;

  assert (host);

  if ((ret = host_is_ipv6_with_port (host, addr, port)) < 0)
    goto cleanup;

  if (ret)
    {
      rv = ret;
      goto cleanup;
    }

  if (strchr (host, ':'))
    {
      struct sockaddr_in6 saddr;
      char *addrptr;
      char *portptr;
      char *lastcolonptr = NULL; /* remove warning */
      char *tmp;

      /* First check, maybe it's a valid IPv6 address straight up */
      if (inet_pton (AF_INET6, host, &saddr) == 1)
        goto out;

      if (!(str = strdup (host)))
        goto cleanup;

      addrptr = str;

      /* find last colon */
      portptr = addrptr;
      while ((tmp = strchr (portptr, ':')))
        {
          lastcolonptr = tmp;
          portptr = tmp + 1;
        }

      *lastcolonptr = '\0';

      /* is everything after the colon a number? */
      tmp = portptr;
      while (isdigit (*tmp))
        tmp++;

      /* are we at the end of the string? */
      /* and did we find a port */
      if (*tmp == '\0' && strlen (portptr))
        {
          is_host_with_port = 1;

          if (addr)
            {
              if (!(*addr = strdup (addrptr)))
                goto cleanup;
            }

          if (port)
            {
              if (!(*port = strdup (portptr)))
                {
                  free (*addr);
                  goto cleanup;
                }
            }
        }
    }

 out:
  rv = is_host_with_port;
 cleanup:
  free (str);
  return (rv);
}

int
host_is_valid (const char *addr, const char *port, uint16_t *portptr)
{
  assert (addr);

  /* achu: max length IPv6 is 45 chars
   * ABCD:ABCD:ABCD:ABCD:ABCD:ABCD:192.168.100.200
   */
  if (strlen (addr) > MAXHOSTNAMELEN)
    return (0);

  if (port)
    {
      char *endptr;
      int tmp;

      errno = 0;
      tmp = strtol (port, &endptr, 0);
      if (errno
          || endptr[0] != '\0'
          || tmp <= 0
          || tmp > USHRT_MAX)
        return 0;

      if (portptr)
        *portptr = tmp;
    }

  return (1);
}

/* Note that we do not do address resolution to map hypothetical
 * situations (e.g. "foobar" resolves to "127.0.0.1").  We only
 * hardcode and check for the known popular strings.
 *
 * The reason is that most of FreeIPMI supports mapping "localhost" to
 * "inband" communication primarily for convenience.  Programmers
 * don't have to handle "inband" communication differently than
 * "outofband" cases.  e.g.  you can script/program with the hosts
 * "node1,node2,node3,localhost,node4" and not have to program a
 * special case for "inband" communication.
 *
 * If a user truly wants some funky host/string to resolve to
 * "localhost", we suggest they use one of the known popular strings
 * instead.  We don't want to have to do host resolution checks for
 * every host/IP ever input into FreeIPMI.
 */
int
host_is_localhost (const char *host)
{
  assert (host);

  /* Ordered by my assumption of most popular */
  if (!strcasecmp (host, "localhost")
      || !strcmp (host, "127.0.0.1")
      || !strcasecmp (host, "ipv6-localhost")
      || !strcmp (host, "::1")
      || !strcasecmp (host, "ip6-localhost")
      || !strcmp (host, "0:0:0:0:0:0:0:1"))
    return (1);

  return (0);
}
