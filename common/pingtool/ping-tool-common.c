/*****************************************************************************\
 *  $Id: ping-tool-common.c,v 1.23 2010-07-08 16:46:10 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2015 Lawrence Livermore National Security, LLC.
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
#include <stdarg.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <netdb.h>
#if HAVE_GETOPT_H
#include <getopt.h>
#endif /* HAVE_GETOPT_H */
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
#include <limits.h>             /* MAXHOSTNAMELEN */
#ifdef HAVE_NETDB_H
#include <netdb.h>              /* MAXHOSTNAMELEN Solaris */
#endif /* HAVE_NETDB_H */
#include <assert.h>
#include <ifaddrs.h>

#include <freeipmi/freeipmi.h>

#include "freeipmi-portability.h"

#include "ping-tool-common.h"

/* achu: max length IPv6 is 45 chars, add +1 for NUL
 * ABCD:ABCD:ABCD:ABCD:ABCD:ABCD:192.168.100.200
 */
#ifndef INET6_ADDRSTRLEN
#define INET6_ADDRSTRLEN 46
#endif

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

#define MAXPORTBUFLEN 16

#define IPMI_PING_MAX_PKT_LEN      1024
#define IPMI_PING_MAX_ERR_LEN      1024

#define IPMI_PING_VERSION_1_5_STR  "1.5"
#define IPMI_PING_VERSION_2_0_STR  "2.0"

#define DEVURANDOM                "/dev/urandom"
#define DEVRANDOM                 "/dev/random"

/* getopt */
extern char *optarg;
extern int optind, opterr, optopt;

/* gethostbyname */
extern int h_errno;

/* cmdline options */
static int pingtool_count = -1;
static int pingtool_interval = 1;
static int pingtool_timeout = 5;
static int pingtool_verbose = 0;
static int pingtool_debug = 0;
static int pingtool_version = IPMI_PING_VERSION_1_5;
static int pingtool_initial_sequence_number = -1;

/* globals */
static int pingtool_sockfd = 0;
static char *pingtool_progname = NULL;
static char *pingtool_interface = NULL;
static char pingtool_dest[MAXHOSTNAMELEN+1];
static char pingtool_dest_ip[INET6_ADDRSTRLEN+1];
static struct sockaddr *pingtool_srcaddr;
static socklen_t pingtool_srcaddr_len;
static struct sockaddr *pingtool_destaddr;
static socklen_t pingtool_destaddr_len;
static struct sockaddr_in pingtool_srcaddr4;
static struct sockaddr_in pingtool_destaddr4;
static struct sockaddr_in6 pingtool_srcaddr6;
static struct sockaddr_in6 pingtool_destaddr6;
static unsigned int pingtool_pkt_sent = 0;
static unsigned int pingtool_pkt_recv = 0;
static Ipmi_Ping_EndResult pingtool_end_result = NULL;

static void
_cleanup (void)
{
  /* ignore potential error, error path */
  close (pingtool_sockfd);
}

static void
_err_init (char *progname)
{
  char *ptr = NULL;

  assert (progname);

  ptr = strrchr (progname, '/');
  pingtool_progname = (ptr == NULL) ? progname : ptr + 1;
}

void
ipmi_ping_err_exit (char *fmt, ...)
{
  char buf[IPMI_PING_MAX_ERR_LEN];
  va_list ap;

  assert (pingtool_progname);
  assert (fmt);

  va_start (ap, fmt);
  snprintf (buf, IPMI_PING_MAX_ERR_LEN, "%s: %s\n", pingtool_progname, fmt);
  vfprintf (stderr, buf, ap);
  va_end (ap);

  _cleanup ();
  exit (EXIT_FAILURE);
}

static void
_strncpy (char *dest, char *src, unsigned int len)
{
  assert (dest);
  assert (src);

  strncpy (dest, src, len);
  dest[len] = '0';
}

static void
_output_usage (const char *options)
{
  assert (pingtool_progname);
  assert (options);

  fprintf (stderr, "%s [OPTIONS] destination\n", pingtool_progname);
  if (strchr (options, 'c'))
    fprintf (stderr, "  -c   count\n");
  if (strchr (options, 'i'))
    fprintf (stderr, "  -i   interval in seconds\n");
  if (strchr (options, 'I'))
    fprintf (stderr, "  -I   interface address or device name\n");
  if (strchr (options, 't'))
    fprintf (stderr, "  -t   timeout in seconds\n");
  if (strchr (options, 'v'))
    fprintf (stderr, "  -v   verbose output\n");
  if (strchr (options, 'r'))
    fprintf (stderr, "  -r   protocol version\n");
  if (strchr (options, 's'))
    fprintf (stderr, "  -s   starting sequence number\n");
  if (strchr (options, 'd'))
    fprintf (stderr, "  -d   turn on debugging\n");
  exit (EXIT_FAILURE);
}

static void
_output_version (void)
{
  assert (pingtool_progname);

  fprintf (stderr, "%s %s\n", pingtool_progname, VERSION);

  exit (EXIT_FAILURE);
}

static void
_cmdline_parse (int argc,
                char **argv,
                unsigned int min_sequence_number,
                unsigned int max_sequence_number,
                const char *options)
{
  char *endptr;
  int c;

  assert (argc >= 0);
  assert (argv);

  /* Turn off error messages */
  opterr = 0;

  while ((c = getopt (argc, argv, options)) != -1)
    {
      switch (c)
        {
        case 'h':
          _output_usage (options);
        case 'V':
          _output_version ();
        case 'r':
          if (!strcmp (optarg, IPMI_PING_VERSION_1_5_STR))
            pingtool_version = IPMI_PING_VERSION_1_5;
          else if (!strcmp (optarg, IPMI_PING_VERSION_2_0_STR))
            pingtool_version = IPMI_PING_VERSION_2_0;
          else
            ipmi_ping_err_exit ("invalid version");
          break;
        case 'c':
          errno = 0;
          pingtool_count = strtol (optarg, &endptr, 10);
          if (errno || endptr[0] != '\0')
            ipmi_ping_err_exit ("count argument invalid");
          if (!pingtool_count)
            ipmi_ping_err_exit ("count must be > 0");
          break;
        case 'i':
          errno = 0;
          pingtool_interval = strtol (optarg, &endptr, 10);
          if (errno || endptr[0] != '\0')
            ipmi_ping_err_exit ("interval argument invalid");
          if (!pingtool_interval)
            ipmi_ping_err_exit ("interval must be > 0");
          break;
        case 'I':
          pingtool_interface = optarg;
          break;
        case 't':
          errno = 0;
          pingtool_timeout = strtol (optarg, &endptr, 10);
          if (errno || endptr[0] != '\0')
            ipmi_ping_err_exit ("timeout argument invalid");
          if (!pingtool_timeout)
            ipmi_ping_err_exit ("timeout must be > 0");
          break;
        case 'v':
          pingtool_verbose++;
          break;
        case 's':
          errno = 0;
          pingtool_initial_sequence_number = strtol (optarg, &endptr, 10);
          if (errno || endptr[0] != '\0')
            ipmi_ping_err_exit ("initial sequence number invalid");
          if (pingtool_initial_sequence_number < min_sequence_number
              || pingtool_initial_sequence_number > max_sequence_number)
            ipmi_ping_err_exit ("initial sequence number out of range");
          break;
        case 'd':
          pingtool_debug++;
          break;
        default:
          ipmi_ping_err_exit ("Command line option error");
          break;
        }
    }

  /* last argument is destination */
  if (optind >= argc)
    ipmi_ping_err_exit ("destination must be specified");

  _strncpy (pingtool_dest, argv[optind], MAXHOSTNAMELEN);
}

/* signal handlers + sleep(3) is a bad idea, so use select(3) */
static int
_sleep (unsigned int len)
{
  struct timeval tv;

  tv.tv_sec = len;
  tv.tv_usec = 0;

  if (select (1, NULL, NULL, NULL, &tv) < 0)
    ipmi_ping_err_exit ("select: %s", strerror (errno));

  return (0);
}

static void
_signal_handler (int sig)
{
  int ret;

  assert (pingtool_progname);
  assert (pingtool_end_result);

  /* Must output result here, b/c who knows where in the code we are
   * when we caught the signal
   */
  ret = pingtool_end_result (pingtool_progname,
                             pingtool_dest,
                             pingtool_pkt_sent,
                             pingtool_pkt_recv);
  _cleanup ();
  exit (ret);
}

static void
_setup (void)
{
  struct addrinfo ai_hints, *ai_res = NULL, *ai = NULL;
  uint16_t port = RMCP_PRIMARY_RMCP_PORT;
  char port_str[MAXPORTBUFLEN + 1];
  int ret;

  if (signal (SIGINT, _signal_handler) == SIG_ERR)
    ipmi_ping_err_exit ("signal setup failed");

  memset (port_str, '\0', MAXPORTBUFLEN + 1);
  snprintf (port_str, MAXPORTBUFLEN, "%d", port);
  memset (&ai_hints, 0, sizeof (struct addrinfo));
  ai_hints.ai_family = AF_UNSPEC;
  ai_hints.ai_socktype = SOCK_DGRAM;
  ai_hints.ai_flags = (AI_V4MAPPED | AI_ADDRCONFIG);

  if ((ret = getaddrinfo (pingtool_dest, port_str, &ai_hints, &ai_res)))
    ipmi_ping_err_exit ("getaddrinfo: %s", gai_strerror (ret));

  /* Try all of the different answers we got, until we succeed. */
  for (ai = ai_res; ai != NULL; ai = ai->ai_next)
    {
      if (ai->ai_family == AF_INET)
        {
          memcpy (&pingtool_destaddr4, ai->ai_addr, ai->ai_addrlen);
          pingtool_destaddr4.sin_family = AF_INET;
          pingtool_destaddr4.sin_port = htons (RMCP_PRIMARY_RMCP_PORT);

	  memset (pingtool_dest_ip, '\0', INET6_ADDRSTRLEN + 1);
          if (!inet_ntop (AF_INET,
                          &pingtool_destaddr4.sin_addr,
                          pingtool_dest_ip,
                          INET6_ADDRSTRLEN))
            ipmi_ping_err_exit ("inet_ntop: %s\n", strerror (errno));

          pingtool_destaddr = (struct sockaddr *)&pingtool_destaddr4;
          pingtool_destaddr_len = sizeof (struct sockaddr_in);

          /* zero everywhere, secure ephemeral port */
          memset (&pingtool_srcaddr4, '\0', sizeof (struct sockaddr_in));
          pingtool_srcaddr4.sin_family = AF_INET;

          pingtool_srcaddr = (struct sockaddr *)&pingtool_srcaddr4;
          pingtool_srcaddr_len = sizeof (struct sockaddr_in);

          if (pingtool_interface)
            {
              struct in_addr in4;

              /* If inet_pton fails, assume its an interface name */
              if (inet_pton (AF_INET,
                             pingtool_interface,
                             &in4) == 1)
                memcpy (&pingtool_srcaddr4.sin_addr,
                        &in4,
                        sizeof (struct in_addr));
              else
                {
                  /* User did not pass in IPv4 or IPv6, so assume it's an interface
                   */
                  struct ifaddrs *ifaddr = NULL, *ifa = NULL;

                  if (getifaddrs (&ifaddr) < 0)
                    ipmi_ping_err_exit ("getifaddrs: %s", strerror (errno));

                  for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next)
                    {
                      if (!ifa->ifa_addr
                          || !ifa->ifa_name)
                        continue;

                      if (!strcmp (ifa->ifa_name, pingtool_interface)
                          && ifa->ifa_addr->sa_family == AF_INET)
                        {
                          struct sockaddr_in tmpaddr;

                          /* memcpy to avoid warnings */
                          memcpy (&tmpaddr, ifa->ifa_addr, sizeof (struct sockaddr_in));

                          memcpy (&pingtool_srcaddr4.sin_addr.s_addr,
                                  &tmpaddr.sin_addr.s_addr,
                                  sizeof (pingtool_srcaddr4.sin_addr.s_addr));
                          break;
                        }

                    }

                  if (!ifa)
                    ipmi_ping_err_exit ("Cannot find interface: %s", pingtool_interface);

                  freeifaddrs (ifaddr);
                }
            }
        }
      else if (ai->ai_family == AF_INET6)
        {
          memcpy (&pingtool_destaddr6, ai->ai_addr, ai->ai_addrlen);
          pingtool_destaddr6.sin6_family = AF_INET6;
          pingtool_destaddr6.sin6_port = htons (RMCP_PRIMARY_RMCP_PORT);

	  memset (pingtool_dest_ip, '\0', INET6_ADDRSTRLEN + 1);
          if (!inet_ntop (AF_INET6,
                          &pingtool_destaddr6.sin6_addr,
                          pingtool_dest_ip,
                          INET6_ADDRSTRLEN))
            ipmi_ping_err_exit ("inet_ntop: %s\n", strerror (errno));

          pingtool_destaddr = (struct sockaddr *)&pingtool_destaddr6;
          pingtool_destaddr_len = sizeof (struct sockaddr_in6);

          /* zero everywhere, secure ephemeral port */
          memset (&pingtool_srcaddr6, '\0', sizeof (struct sockaddr_in6));
          pingtool_srcaddr6.sin6_family = AF_INET6;

          pingtool_srcaddr = (struct sockaddr *)&pingtool_srcaddr6;
          pingtool_srcaddr_len = sizeof (struct sockaddr_in6);

          if (pingtool_interface)
            {
	      struct ifaddrs *ifaddr = NULL, *ifa = NULL;
              struct in6_addr in6;
	      int is_interfacename = 0;

              /* If inet_pton fails, assume its an interface name */
              if (inet_pton (AF_INET6,
                             pingtool_interface,
                             &in6) != 1)
		is_interfacename = 1;

	      if (getifaddrs (&ifaddr) < 0)
		ipmi_ping_err_exit ("getifaddrs: %s", strerror (errno));

	      for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next)
		{
		  if (!ifa->ifa_addr
		      || !ifa->ifa_name)
		    continue;

		  if (ifa->ifa_addr->sa_family == AF_INET6)
		    {
		      struct sockaddr_in6 tmpaddr;

		      /* memcpy to avoid warnings */
		      memcpy (&tmpaddr, ifa->ifa_addr, sizeof (struct sockaddr_in6));

		      if ((is_interfacename && !strcmp (ifa->ifa_name, pingtool_interface))
			  || !memcmp (&tmpaddr.sin6_addr, &in6, sizeof (struct in6_addr)))
                        {
                          memcpy (&pingtool_srcaddr6.sin6_addr,
                                  &tmpaddr.sin6_addr,
                                  sizeof (pingtool_srcaddr6.sin6_addr));

			  memcpy (&pingtool_srcaddr6.sin6_scope_id,
				  &tmpaddr.sin6_scope_id,
				  sizeof (pingtool_srcaddr6.sin6_scope_id));
                          break;
                        }
                    }

                  if (!ifa)
                    ipmi_ping_err_exit ("Cannot find interface: %s", pingtool_interface);
		}

	      freeifaddrs (ifaddr);
            }
        }
      else
        continue;

      if ((pingtool_sockfd = socket (ai->ai_family,
                                     ai->ai_socktype,
                                     ai->ai_protocol)) < 0)
        ipmi_ping_err_exit ("socket: %s", strerror (errno));

      if (bind (pingtool_sockfd, pingtool_srcaddr, pingtool_srcaddr_len) < 0)
        {
          close (pingtool_sockfd);
          continue;
        }

      break;
    }

  if (!ai)
    ipmi_ping_err_exit ("Error determining destination IP");

  srand (time (NULL));

  freeaddrinfo (ai_res);
}

static void
_main_loop (Ipmi_Ping_CreatePacket create,
            Ipmi_Ping_ParsePacket parse,
            Ipmi_Ping_LatePacket late)
{
  unsigned int sequence_number = 0;
  time_t last_send = 0;
  int ret;

  assert (create);
  assert (parse);
  assert (late);
  assert (pingtool_progname);
  assert (pingtool_end_result);

  if (pingtool_initial_sequence_number < 0)
    {
      int len;

      if ((len = ipmi_get_random (&pingtool_initial_sequence_number,
                                  sizeof (pingtool_initial_sequence_number))) < 0)
        ipmi_ping_err_exit ("ipmi_get_random: %s", strerror (errno));
      if (len != sizeof (pingtool_initial_sequence_number))
        ipmi_ping_err_exit ("ipmi_get_random: invalid len returned");
    }

  sequence_number = pingtool_initial_sequence_number;

  printf ("%s %s (%s)\n", pingtool_progname, pingtool_dest, pingtool_dest_ip);

  while (pingtool_count == -1 || (pingtool_pkt_sent < pingtool_count))
    {
      int rv, len, received = 0;
      uint8_t buf[IPMI_PING_MAX_PKT_LEN];
      time_t now;

      /* wait if necessary */
      now = time (NULL);
      if ((now - last_send) < pingtool_interval)
        {
          if (_sleep ((last_send + pingtool_interval - now)) < 0)
            continue;
        }

      if ((len = create (pingtool_dest,
                         buf,
                         IPMI_PING_MAX_PKT_LEN,
                         sequence_number,
                         pingtool_version,
                         pingtool_debug)) < 0)
        ipmi_ping_err_exit ("_create failed: %s", strerror (errno));

      rv = ipmi_lan_sendto (pingtool_sockfd,
                            buf,
                            len,
                            0,
                            pingtool_destaddr,
                            pingtool_destaddr_len);
      if (rv < 0)
        ipmi_ping_err_exit ("ipmi_sendto: %s", strerror (errno));

      if (rv != len)
        ipmi_ping_err_exit ("ipmi_sendto: wrong bytes written");

      last_send = time (NULL);

      pingtool_pkt_sent++;

      while (((now = time (NULL)) - last_send) < pingtool_timeout)
        {
          fd_set rset;
          struct timeval tv;

          FD_ZERO (&rset);
          FD_SET (pingtool_sockfd, &rset);

          tv.tv_sec = (last_send + pingtool_timeout - now);
          tv.tv_usec = 0;

          if ((rv = select (pingtool_sockfd+1, &rset, NULL, NULL, &tv)) < 0)
            ipmi_ping_err_exit ("select: %s", strerror (errno));

          if (rv == 1)
            {
              struct sockaddr_in6 from6;
              socklen_t fromlen;
              char fromstr[INET6_ADDRSTRLEN+1];

              fromlen = sizeof (from6);
              len = ipmi_lan_recvfrom (pingtool_sockfd,
                                       buf,
                                       IPMI_PING_MAX_PKT_LEN,
                                       0,
                                       (struct sockaddr *)&from6,
                                       &fromlen);

              /* achu & hliebig:
               *
               * Premise from ipmitool (http://ipmitool.sourceforge.net/)
               *
               * On some OSes (it seems Unixes), the behavior is to not return
               * port denied errors up to the client for UDP responses (i.e. you
               * need to timeout).  But on some OSes (it seems Windows), the
               * behavior is to return port denied errors up to the user for UDP
               * responses via ECONNRESET or ECONNREFUSED.
               *
               * If this were just the case, we could return or handle errors
               * properly and move on.  However, it's not the case.
               *
               * According to Ipmitool, on some motherboards, both the OS and the
               * BMC are capable of responding to an IPMI request.  That means you
               * can get an ECONNRESET or ECONNREFUSED, then later on, get your
               * real IPMI response.
               *
               * Our solution is copied from Ipmitool, we'll ignore some specific
               * errors and try to read again.
               *
               * If the ECONNREFUSED or ECONNRESET is from the OS, but we will get
               * an IPMI response later, the recvfrom later on gets the packet we
               * want.
               *
               * If the ECONNREFUSED or ECONNRESET is from the OS but there is no
               * BMC (or IPMI disabled, etc.), just do the recvfrom again to
               * eventually get a timeout, which is the behavior we'd like.
               */

              if (len < 0
                  && (errno == ECONNRESET
                      || errno == ECONNREFUSED))
                continue;

              if (len < 0)
                ipmi_ping_err_exit ("ipmi_recvfrom: %s", strerror (errno));

	      memset (fromstr, '\0', INET6_ADDRSTRLEN + 1);
              if (from6.sin6_family == AF_INET6)
                {
                  if (!inet_ntop (AF_INET6, &from6.sin6_addr, fromstr, INET6_ADDRSTRLEN))
                    ipmi_ping_err_exit ("inet_ntop: %s\n", strerror (errno));
                }
              else
                {
                  /* memcpy hacks to avoid warnings, i.e.
                   * warning: dereferencing pointer 'X' does break strict-aliasing rules
                   */
                  struct sockaddr_in from4;

                  memcpy (&from4, &from6, fromlen);

                  if (!inet_ntop (AF_INET, &from4.sin_addr, fromstr, INET6_ADDRSTRLEN))
                    ipmi_ping_err_exit ("inet_ntop: %s\n", strerror (errno));
                }

              if ((rv = parse (pingtool_dest,
                               buf,
                               len,
                               fromstr,
                               sequence_number,
                               pingtool_verbose,
                               pingtool_version,
                               pingtool_debug)) < 0)
                ipmi_ping_err_exit ("_parse failed: %s", strerror (errno));

              /* If rv == 0, the sequence numbers don't match, so
               * we'll wait some more for the latest packet we sent
               * out.
               */
              if (!rv)
                continue;

              received++;
              pingtool_pkt_recv++;
              break;
            }
        }

      if (!received)
        late (sequence_number);

      sequence_number++;
    }

  ret = pingtool_end_result (pingtool_progname,
                             pingtool_dest,
                             pingtool_pkt_sent,
                             pingtool_pkt_recv);
  _cleanup ();
  exit (ret);
}

void
ipmi_ping_setup (int argc,
                 char **argv,
                 unsigned int min_sequence_number,
                 unsigned int max_sequence_number,
                 const char *options)
{
  char *valid_options = "hVciItvrsd:";
  char *ptr;
  char c;

  assert (argc >= 0);
  assert (argv);
  assert (options);

  /* Check for valid options */
  ptr = (char *)options;
  while ((c = *ptr))
    {
      if (!strchr (valid_options, c))
        {
          fprintf (stderr, "ipmi_ping_setup: invalid options listed");
          exit (EXIT_FAILURE);
        }
      ptr++;
    }

  _err_init (argv[0]);
  _cmdline_parse (argc,
                  argv,
                  min_sequence_number,
                  max_sequence_number,
                  options);
  _setup ();
}

void
ipmi_ping_loop (Ipmi_Ping_CreatePacket create,
                Ipmi_Ping_ParsePacket parse,
                Ipmi_Ping_LatePacket late,
                Ipmi_Ping_EndResult end)
{
  assert (create);
  assert (parse);
  assert (late);
  assert (end);

  pingtool_end_result = end;

  _main_loop (create, parse, late);

  return;                     /* NOT REACHED */
}
