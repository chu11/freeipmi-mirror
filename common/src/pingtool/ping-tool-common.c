/*****************************************************************************\
 *  $Id: ping-tool-common.c,v 1.23 2010-07-08 16:46:10 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2011 Lawrence Livermore National Security, LLC.
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
#include <sys/ioctl.h>
#if HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif /* HAVE_SYS_SOCKIO_H */
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

#include <freeipmi/freeipmi.h>

#include "freeipmi-portability.h"

#include "ping-tool-common.h"

#ifndef INET_ADDRSTRLEN
#define INET_ADDRSTRLEN 16
#endif

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

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
static int _count = -1;
static int _interval = 1;
static int _timeout = 5;
static int _verbose = 0;
static int _debug = 0;
static int _version = IPMI_PING_VERSION_1_5;
static int _initial_sequence_number = -1;

/* globals */
static int _sockfd = 0;
static char *_progname = NULL;
static char *_interface = NULL;
static char _dest[MAXHOSTNAMELEN+1];
static char _dest_ip[INET_ADDRSTRLEN+1];
static struct sockaddr_in _srcaddr;
static struct sockaddr_in _destaddr;
static unsigned int _pkt_sent = 0;
static unsigned int _pkt_recv = 0;
static Ipmi_Ping_EndResult _end_result = NULL;

static void
_cleanup (void)
{
  /* ignore potential error, error path */
  close (_sockfd);
}

static void
_err_init (char *__progname)
{
  char *ptr = NULL;

  assert (__progname);

  ptr = strrchr (__progname, '/');
  _progname = (ptr == NULL) ? __progname : ptr + 1;
}

void
ipmi_ping_err_exit (char *fmt, ...)
{
  char buf[IPMI_PING_MAX_ERR_LEN];

  if (!fmt || !_progname)
    fprintf (stderr, "ipmi_ping_err_exit: improperly called\n");
  else
    {
      va_list ap;
      va_start (ap, fmt);
      snprintf (buf, IPMI_PING_MAX_ERR_LEN, "%s: %s\n", _progname, fmt);
      vfprintf (stderr, buf, ap);
      va_end (ap);
    }
  _cleanup ();
  exit (1);
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
  assert (_progname);

  fprintf (stderr, "%s [OPTIONS] destination\n", _progname);
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
  exit (1);
}

static void
_output_version (void)
{
  assert (_progname);

  fprintf (stderr, "%s %s\n", _progname, VERSION);

  exit (1);
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
            _version = IPMI_PING_VERSION_1_5;
          else if (!strcmp (optarg, IPMI_PING_VERSION_2_0_STR))
            _version = IPMI_PING_VERSION_2_0;
          else
            ipmi_ping_err_exit ("invalid version");
          break;
        case 'c':
          _count = strtol (optarg, &endptr, 10);
	  if (errno || endptr[0] != '\0')
            ipmi_ping_err_exit ("count argument invalid");
          if (!_count)
            ipmi_ping_err_exit ("count must be > 0");
          break;
        case 'i':
          _interval = strtol (optarg, &endptr, 10);
	  if (errno || endptr[0] != '\0')
            ipmi_ping_err_exit ("interval argument invalid");
          if (!_interval)
            ipmi_ping_err_exit ("interval must be > 0");
          break;
        case 'I':
          _interface = optarg;
          break;
        case 't':
          _timeout = strtol (optarg, &endptr, 10);
	  if (errno || endptr[0] != '\0')
            ipmi_ping_err_exit ("timeout argument invalid");
          if (!_timeout)
            ipmi_ping_err_exit ("timeout must be > 0");
          break;
        case 'v':
          _verbose++;
          break;
        case 's':
          _initial_sequence_number = strtol (optarg, &endptr, 10);
	  if (errno || endptr[0] != '\0')
            ipmi_ping_err_exit ("initial sequence number invalid");
          if (_initial_sequence_number < min_sequence_number
              || _initial_sequence_number > max_sequence_number)
            ipmi_ping_err_exit ("initial sequence number out of range");
          break;
        case 'd':
          _debug++;
          break;
        default:
          ipmi_ping_err_exit ("Command line option error");
          break;
        }
    }

  /* last argument is destination */
  if (optind >= argc)
    ipmi_ping_err_exit ("destination must be specified");

  _strncpy (_dest, argv[optind], MAXHOSTNAMELEN);
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

  assert (_progname);
  assert (_end_result);

  /* Must output result here, b/c who knows where in the code we are
   * when we caught the signal
   */
  ret = _end_result (_progname, _dest, _pkt_sent, _pkt_recv);
  _cleanup ();
  exit (ret);
}

static void
_setup (void)
{
  struct hostent *hptr;
  char *temp;

  if (signal (SIGINT, _signal_handler) == SIG_ERR)
    ipmi_ping_err_exit ("signal setup failed");

  if ((_sockfd = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
    ipmi_ping_err_exit ("socket: %s", strerror (errno));

  memset (&_srcaddr, '\0', sizeof (_srcaddr));
  _srcaddr.sin_family = AF_INET;
  _srcaddr.sin_port = htons (0);

  if (!_interface)
    _srcaddr.sin_addr.s_addr = htonl (INADDR_ANY);
  else
    {
      /* If there is a period, assume user input an IP address.  No
       * period, assume user input an interface name
       */
      if (strchr (_interface, '.'))
        {
          int rv;

          if ((rv = inet_pton (AF_INET, _interface, &_srcaddr.sin_addr)) < 0)
            ipmi_ping_err_exit ("inet_pton: %s", strerror (errno));
          if (!rv)
            ipmi_ping_err_exit ("invalid interface address");
        }
      else
        {
          struct ifreq ifr;
          struct sockaddr_in temp_sockaddr;

          _strncpy (ifr.ifr_name, _interface, IFNAMSIZ);
          ifr.ifr_addr.sa_family = AF_INET;
          if (ioctl (_sockfd, SIOCGIFADDR, &ifr) < 0)
            ipmi_ping_err_exit ("ioctl: %s", strerror (errno));

          temp_sockaddr = *((struct sockaddr_in *)&ifr.ifr_addr);
          memcpy (&_srcaddr.sin_addr.s_addr, &temp_sockaddr.sin_addr.s_addr,
                  sizeof (_srcaddr.sin_addr.s_addr));
        }
    }

  if (bind (_sockfd, (struct sockaddr *)&_srcaddr, sizeof (_srcaddr)) < 0)
    ipmi_ping_err_exit ("bind: %s", strerror (errno));

  memset (&_destaddr, '\0', sizeof (_destaddr));
  _destaddr.sin_family = AF_INET;
  _destaddr.sin_port = htons (RMCP_PRIMARY_RMCP_PORT);

  if (!(hptr = gethostbyname (_dest)))
    {
#if HAVE_HSTRERROR
      ipmi_ping_err_exit ("gethostbyname: %s", hstrerror (h_errno));
#else /* !HAVE_HSTRERROR */
      ipmi_ping_err_exit ("gethostbyname: h_errno = %d", h_errno);
#endif /* !HAVE_HSTRERROR */
    }
  _destaddr.sin_addr = *((struct in_addr *)hptr->h_addr);
  temp = inet_ntoa (_destaddr.sin_addr);
  _strncpy (_dest_ip, temp, INET_ADDRSTRLEN);

  srand (time (NULL));
}

static void
_main_loop (Ipmi_Ping_CreatePacket _create,
            Ipmi_Ping_ParsePacket _parse,
            Ipmi_Ping_LatePacket _late)
{
  unsigned int sequence_number = 0;
  time_t last_send = 0;
  int ret;

  assert (_create);
  assert (_parse);
  assert (_late);
  assert (_progname);
  assert (_end_result);

  if (_initial_sequence_number < 0)
    {
      int len;

      if ((len = ipmi_get_random (&_initial_sequence_number,
                                  sizeof (_initial_sequence_number))) < 0)
        ipmi_ping_err_exit ("ipmi_get_random: %s", strerror (errno));
      if (len != sizeof (_initial_sequence_number))
        ipmi_ping_err_exit ("ipmi_get_random: invalid len returned");
    }

  sequence_number = _initial_sequence_number;

  printf ("%s %s (%s)\n", _progname, _dest, _dest_ip);

  while (_count == -1 || (_pkt_sent < _count))
    {
      int rv, len, received = 0;
      uint8_t buf[IPMI_PING_MAX_PKT_LEN];
      time_t now;

      /* wait if necessary */
      now = time (NULL);
      if ((now - last_send) < _interval)
        {
          if (_sleep ((last_send + _interval - now)) < 0)
            continue;
        }

      if ((len = _create (_dest,
                          buf,
                          IPMI_PING_MAX_PKT_LEN,
                          sequence_number,
                          _version,
                          _debug)) < 0)
        ipmi_ping_err_exit ("_create failed: %s", strerror (errno));

      rv = ipmi_lan_sendto (_sockfd,
                            buf,
                            len,
                            0,
                            (struct sockaddr *)&_destaddr,
                            sizeof (_destaddr));
      if (rv < 0)
        ipmi_ping_err_exit ("ipmi_sendto: %s", strerror (errno));

      if (rv != len)
        ipmi_ping_err_exit ("ipmi_sendto: wrong bytes written");

      last_send = time (NULL);

      _pkt_sent++;

      while (((now = time (NULL)) - last_send) < _timeout)
        {
          fd_set rset;
          struct timeval tv;

          FD_ZERO (&rset);
          FD_SET (_sockfd, &rset);

          tv.tv_sec = (last_send + _timeout - now);
          tv.tv_usec = 0;

          if ((rv = select (_sockfd+1, &rset, NULL, NULL, &tv)) < 0)
            ipmi_ping_err_exit ("select: %s", strerror (errno));

          if (rv == 1)
            {
              struct sockaddr_in from;
              socklen_t fromlen;

              fromlen = sizeof (from);
              len = ipmi_lan_recvfrom (_sockfd,
                                       buf,
                                       IPMI_PING_MAX_PKT_LEN,
                                       0,
                                       (struct sockaddr *)&from,
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

              if ((rv = _parse (_dest,
                                buf,
                                len,
                                inet_ntoa (from.sin_addr),
                                sequence_number,
                                _verbose,
                                _version,
                                _debug)) < 0)
                ipmi_ping_err_exit ("_parse failed: %s", strerror (errno));

              /* If rv == 0, the sequence numbers don't match, so
               * we'll wait some more for the latest packet we sent
               * out.
               */
              if (!rv)
                continue;

              received++;
              _pkt_recv++;
              break;
            }
        }

      if (!received)
        _late (sequence_number);

      sequence_number++;
    }

  ret = _end_result (_progname, _dest, _pkt_sent, _pkt_recv);
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

  if (argc <= 0 || !argv || !options)
    {
      fprintf (stderr, "ipmi_ping_setup: called improperly\n");
      exit (1);
    }

  /* Check for valid options */
  ptr = (char *)options;
  while ((c = *ptr))
    {
      if (!strchr (valid_options, c))
        {
          fprintf (stderr, "ipmi_ping_setup: invalid options listed");
          exit (1);
        }
      ptr++;
    }

  _err_init (argv[0]);
  _cmdline_parse (argc, argv, min_sequence_number, max_sequence_number, options);
  _setup ();
}

void
ipmi_ping_loop (Ipmi_Ping_CreatePacket _create,
                Ipmi_Ping_ParsePacket _parse,
                Ipmi_Ping_LatePacket _late,
                Ipmi_Ping_EndResult _end)
{
  if (!_create || !_parse || !_late || !_end)
    {
      fprintf (stderr, "ipmi_ping_loop: called improperly\n");
      exit (1);
    }

  _end_result = _end;

  _main_loop (_create, _parse, _late);

  return;                     /* NOT REACHED */
}
