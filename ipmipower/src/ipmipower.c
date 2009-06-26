/*****************************************************************************\
 *  $Id: ipmipower.c,v 1.83 2009-06-26 02:32:06 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 2 of the License, or (at your
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
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
#include <stdint.h>
#include <sys/stat.h>
#include <sys/resource.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <netinet/in.h>
#include <errno.h>

#include "ipmipower.h"
#include "ipmipower_argp.h"
#include "ipmipower_connection.h"
#include "ipmipower_error.h"
#include "ipmipower_powercmd.h"
#include "ipmipower_prompt.h"
#include "ipmipower_ping.h"
#include "ipmipower_util.h"

#include "ierror.h"

#include "freeipmi-portability.h"
#include "cbuf.h"
#include "hostlist.h"
#include "tool-common.h"

cbuf_t ttyin;
cbuf_t ttyout;
cbuf_t ttyerr;

/* configuration for ipmipower */
struct ipmipower_arguments cmd_args;

/* Array of all ipmi "connections" */
struct ipmipower_connection *ics = NULL;
unsigned int ics_len = 0;

/* Array of hostlists for short output */
int output_hostrange_flag = 0;
hostlist_t output_hostrange[MSG_TYPE_NUM_ENTRIES];

/* _setup
 * - Setup structures and values for the program
 */
static int
_setup (void)
{
  int i;
  struct rlimit rlim;

  /* Make best effort to increase file descriptor limit, if it fails
   * for any reason, don't worry about it, its no big deal.
   */

  if (getrlimit (RLIMIT_NOFILE, &rlim) == 0)
    {
      rlim.rlim_cur = rlim.rlim_max;
      setrlimit (RLIMIT_NOFILE,&rlim);
    }

  if (ipmi_rmcpplus_init () < 0)
    {
      IPMIPOWER_ERROR (("ipmi_rmcpplus_init: %s", strerror (errno)));
      return (-1);
    }

  /* Create TTY bufs */
  if (!(ttyin  = cbuf_create (IPMIPOWER_MIN_TTY_BUF, IPMIPOWER_MAX_TTY_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      return (-1);
    }
  cbuf_opt_set (ttyin, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);

  if (!(ttyout = cbuf_create (IPMIPOWER_MIN_TTY_BUF, IPMIPOWER_MAX_TTY_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      return (-1);
    }
  cbuf_opt_set (ttyout, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);

  if (!(ttyerr = cbuf_create (IPMIPOWER_MIN_TTY_BUF, IPMIPOWER_MAX_TTY_BUF)))
    {
      IPMIPOWER_ERROR (("cbuf_create: %s", strerror (errno)));
      return (-1);
    }
  cbuf_opt_set (ttyerr, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);

  for (i = 0; i < MSG_TYPE_NUM_ENTRIES; i++)
    {
      if (!(output_hostrange[i] = hostlist_create (NULL)))
        {
          IPMIPOWER_ERROR (("hostlist_create: %s", strerror (errno)));
          return (-1);
        }
    }

  return (0);
}

/* _cleanup
 * - Cleanup dynamic memory allocated in this program
 */
static void
_cleanup (void)
{
  int i;

  cbuf_destroy (ttyin);

  /* Flush before destroying. */
  cbuf_read_to_fd (ttyout, STDOUT_FILENO, -1);
  cbuf_destroy (ttyout);
  cbuf_read_to_fd (ttyerr, STDERR_FILENO, -1);
  cbuf_destroy (ttyerr);

  ipmipower_connection_array_destroy (ics, ics_len);

  for (i = 0; i < MSG_TYPE_NUM_ENTRIES; i++)
    hostlist_destroy (output_hostrange[i]);
}

static int
_eliminate_nodes (void)
{
  if (cmd_args.hostrange.eliminate)
    {
      ipmidetect_t id = NULL;
      int i;

      if (!(id = ipmidetect_handle_create ()))
        {
          IPMIPOWER_ERROR (("ipmidetect_handle_create: %s", strerror (errno)));
          return (-1);
        }

      if (ipmidetect_load_data (id,
                                NULL,
                                0,
                                0) < 0)
        {
          if (ipmidetect_errnum (id) == IPMIDETECT_ERR_CONNECT
              || ipmidetect_errnum (id) == IPMIDETECT_ERR_CONNECT_TIMEOUT)
            IPMIPOWER_ERROR (("Error connecting to ipmidetect daemon"));
          else
            IPMIPOWER_ERROR (("ipmidetect_load_data: %s", ipmidetect_errormsg (id)));
          return (-1);
        }

      for (i = 0; i < ics_len; i++)
        {
          int ret;
          
          if ((ret = ipmidetect_is_node_detected (id, ics[i].hostname)) < 0)
            {
              if (ipmidetect_errnum (id) == IPMIDETECT_ERR_NOTFOUND)
                IPMIPOWER_ERROR (("Node '%s' unrecognized by ipmidetect", ics[i].hostname));
              else
                IPMIPOWER_ERROR (("ipmidetect_is_node_detected: %s", ipmidetect_errormsg (id)));
              return (-1);
            }

          if (!ret)
            ics[i].skip++;
        }

      ipmidetect_handle_destroy (id);
    }

  return (0);
}

static void
_sendto (cbuf_t cbuf, int fd, struct sockaddr_in *destaddr)
{
  int n, rv;
  uint8_t buf[IPMIPOWER_PACKET_BUFLEN];

  if ((n = cbuf_read (cbuf, buf, IPMIPOWER_PACKET_BUFLEN)) < 0)
    ierr_exit ("cbuf_read: %s", fd, strerror (errno));
  if (n == IPMIPOWER_PACKET_BUFLEN)
    ierr_exit ("cbuf_read: Buffer full");

  do 
    {
      rv = ipmi_lan_sendto (fd,
                            buf,
                            n,
                            0,
                            (struct sockaddr *)destaddr,
                            sizeof (struct sockaddr_in));
    } while (rv < 0 && errno == EINTR);

  if (rv < 0)
    ierr_exit ("ipmi_lan_sendto %s", strerror (errno));

#if 0
  /* don't check, let bad packet timeout */
  if (rv != n)
    ierr_exit ("ipmi_lan_sendto rv=%d n=%d", rv, n);
#endif

  /* cbuf should be empty now */
  if (!cbuf_is_empty (cbuf))
    ierr_exit ("cbuf not empty");
}

static void
_recvfrom (cbuf_t cbuf, int fd, struct sockaddr_in *srcaddr)
{
  int n, rv, dropped = 0;
  uint8_t buf[IPMIPOWER_PACKET_BUFLEN];
  struct sockaddr_in from;
  unsigned int fromlen = sizeof (struct sockaddr_in);

  do
    {
      rv = ipmi_lan_recvfrom (fd,
                              buf,
                              IPMIPOWER_PACKET_BUFLEN,
                              0,
                              (struct sockaddr *)&from, &fromlen);
    } while (rv < 0 && errno == EINTR);

  if (rv < 0)
    ierr_exit ("ipmi_lan_recvfrom: %s", strerror (errno));

  if (!rv)
    ierr_exit ("ipmi_lan_recvfrom: EOF");

  /* Don't store if this is packet is strange for some reason */
  if (from.sin_family != AF_INET
      || from.sin_addr.s_addr != srcaddr->sin_addr.s_addr)
    return;

  /* cbuf should be empty, but if it isn't, empty it */
  if (!cbuf_is_empty (cbuf))
    {
      IPMIPOWER_DEBUG (("cbuf not empty, draining"));

      do
        {
          uint8_t tempbuf[IPMIPOWER_PACKET_BUFLEN];

          if (cbuf_read (cbuf, tempbuf, IPMIPOWER_PACKET_BUFLEN) < 0)
            ierr_exit ("cbuf_read: %s", strerror (errno));
        } while(!cbuf_is_empty (cbuf));
    }

  if ((n = cbuf_write (cbuf, buf, rv, &dropped)) < 0)
    ierr_exit ("cbuf_write: %s", strerror (errno));

  if (n != rv)
    ierr_exit ("cbuf_write: rv=%d n=%d", rv, n);

  if (dropped)
    IPMIPOWER_DEBUG (("cbuf_write: read dropped %d bytes", dropped));
}

/* _poll_loop
 * - poll on all descriptors
 */
static void
_poll_loop (int non_interactive)
{
  int nfds = 0;
  struct pollfd *pfds = NULL;

  while (non_interactive || ipmipower_prompt_process_cmdline ())
    {
      int i, num, timeout;
      int powercmd_timeout = -1;
      int ping_timeout = -1;

      /* If there are no pending commands before this call,
       * powercmd_timeout will not be set, leaving it at -1
       */
      num = ipmipower_powercmd_process_pending (&powercmd_timeout);
      if (non_interactive && !num)
        break;

      /* ping timeout is always set if cmd_args.ping_interval > 0 */
      ipmipower_ping_process_pings (&ping_timeout);

      if (cmd_args.ping_interval)
        {
          if (powercmd_timeout == -1)
            timeout = ping_timeout;
          else
            timeout = (ping_timeout < powercmd_timeout) ?
          ping_timeout : powercmd_timeout;
        }
      else
        timeout = powercmd_timeout;

      /* achu: I always wonder if this poll() loop could be done far
       * more elegantly and efficiently without all this crazy
       * indexing, perhaps through a callback/event mechanism.  It'd
       * probably be more efficient, since most callback/event based
       * models have min-heap like structures inside for determining
       * what things timed out. Overall though, I don't think the O(n)
       * (n being hosts/fds) processing is really that inefficient for
       * this particular application and is not worth going back and
       * changing.  By going to a callback/event mechanism, there will
       * still be some O(n) activities within the code, so I am only
       * going to create a more efficient O(n) poll loop.
       */

      /* Has the number of hosts changed? */
      if (nfds != (ics_len*2) + 3)
        {
          /* The "*2" is for each host's two fds, one for ipmi
           * (ipmi_fd) and one for rmcp (ping_fd).  The "+3" is for
           * stdin, stdout, stderr.
           */
          nfds = (ics_len*2) + 3;
          free (pfds);
          if (!(pfds = (struct pollfd *)malloc (nfds * sizeof (struct pollfd))))
            ierr_exit ("malloc: %s", strerror (errno));
        }

      for (i = 0; i < ics_len; i++)
        {
          pfds[i*2].fd = ics[i].ipmi_fd;
          pfds[i*2+1].fd = ics[i].ping_fd;
          pfds[i*2].events = pfds[i*2+1].events = 0;
          pfds[i*2].revents = pfds[i*2+1].revents = 0;

          pfds[i*2].events |= POLLIN;
          if (!cbuf_is_empty (ics[i].ipmi_out))
            pfds[i*2].events |= POLLOUT;

          if (!cmd_args.ping_interval)
            continue;

          pfds[i*2+1].events |= POLLIN;
          if (!cbuf_is_empty (ics[i].ping_out))
            pfds[i*2+1].events |= POLLOUT;
        }

      pfds[nfds-3].fd = STDIN_FILENO;
      pfds[nfds-3].events = POLLIN;
      pfds[nfds-3].revents = 0;
      pfds[nfds-2].fd = STDOUT_FILENO;
      if (!cbuf_is_empty (ttyout))
        pfds[nfds-2].events = POLLOUT;
      else
        pfds[nfds-2].events = 0;
      pfds[nfds-2].revents = 0;
      pfds[nfds-1].fd = STDERR_FILENO;
      if (!cbuf_is_empty (ttyerr))
        pfds[nfds-1].events = POLLOUT;
      else
        pfds[nfds-1].events = 0;
      pfds[nfds-1].revents = 0;

      ipmipower_poll (pfds, nfds, timeout);

      for (i = 0; i < ics_len; i++)
        {
          if (pfds[i*2].revents & POLLERR)
            {
              IPMIPOWER_DEBUG (("host = %s; IPMI POLLERR", ics[i].hostname));
              continue;
            }

          if (pfds[i*2].revents & POLLIN)
            _recvfrom (ics[i].ipmi_in, ics[i].ipmi_fd, &(ics[i].destaddr));

          if (pfds[i*2].revents & POLLOUT)
            _sendto (ics[i].ipmi_out, ics[i].ipmi_fd, &(ics[i].destaddr));

          if (!cmd_args.ping_interval)
            continue;

          if (pfds[i*2+1].revents & POLLERR)
            {
              IPMIPOWER_DEBUG (("host = %s; PING_POLLERR", ics[i].hostname));
              continue;
            }

          if (pfds[i*2+1].revents & POLLIN)
            _recvfrom (ics[i].ping_in, ics[i].ping_fd, &(ics[i].destaddr));

          if (pfds[i*2+1].revents & POLLOUT)
            _sendto (ics[i].ping_out, ics[i].ping_fd, &(ics[i].destaddr));
        }

      if (pfds[nfds-3].revents & POLLIN)
        {
          int n, dropped = 0;

          if ((n = cbuf_write_from_fd (ttyin, STDIN_FILENO, -1, &dropped)) < 0)
            ierr_exit ("cbuf_write_from_fd: %s", strerror (errno));

          /* achu: If you are running ipmipower in co-process mode with
           * powerman, this error condition will probably be hit with the file
           * descriptor STDIN_FILENO.  The powerman daemon is usually closed
           * by /etc/init.d/powerman stop, which kills a process through a
           * signal.  Thus, powerman closes stdin and stdout pipes to
           * ipmipower and the call to cbuf_write_from_fd will give us an EOF
           * reading.  We'll consider this EOF an "ok" error.
           */
          if (!n)
            exit (1);
          
          if (dropped)
            ierr_dbg ("cbuf_write_from_fd: read dropped %d bytes", dropped);
        }
      if (!cbuf_is_empty (ttyout) && (pfds[nfds-2].revents & POLLOUT))
        {
          if (cbuf_read_to_fd (ttyout, STDOUT_FILENO, -1) < 0)
            ierr_exit ("cbuf_read_to_fd: %s", strerror (errno));
        }
      if (!cbuf_is_empty (ttyerr) && (pfds[nfds-1].revents & POLLOUT))
        {
          if (cbuf_read_to_fd (ttyerr, STDERR_FILENO, -1) < 0)
            ierr_exit ("cbuf_read_to_fd: %s", strerror (errno));
        }
    }

  free (pfds);
}

int
main (int argc, char *argv[])
{
  int exit_code;

  ipmi_disable_coredump ();

  ipmipower_argp_parse (argc, argv, &cmd_args);

  /* Call post argp parsing */
  ierr_init (argv[0]);
  ierr_file_descriptor (1, STDERR_FILENO); /* initially errors goto stderr */

  if (cmd_args.powercmd == POWER_CMD_NONE)
    ipmipower_error_setup (IPMIPOWER_ERROR_STDERR | IPMIPOWER_ERROR_SYSLOG);
  else
    ipmipower_error_setup (IPMIPOWER_ERROR_STDERR);
  
  if (_setup () < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (ipmipower_powercmd_setup () < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (cmd_args.common.hostname)
    {
      unsigned int len = 0;

      if (!(ics = ipmipower_connection_array_create (cmd_args.common.hostname, &len)))
        {
          /* dump error outputs here, most notably invalid hostname output */
          cbuf_read_to_fd (ttyout, STDOUT_FILENO, -1);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      ics_len = len;
    }

  /* If power command (i.e. --reset, --stat, etc.) is passed at
   * command line, put the power control commands in the pending
   * queue.
   */
  if (cmd_args.powercmd != POWER_CMD_NONE)
    {
      int i;

      cmd_args.ping_interval = 0;

      /* Check for appropriate privilege first */
      if (cmd_args.common.privilege_level == IPMI_PRIVILEGE_LEVEL_USER
          && POWER_CMD_REQUIRES_OPERATOR_PRIVILEGE_LEVEL (cmd_args.powercmd))
        {
          IPMIPOWER_ERROR (("power operation requires atleast operator privilege"));
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (_eliminate_nodes () < 0)
        {
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      for (i = 0; i < ics_len; i++)
        {
          if (ics[i].skip)
            continue;

          ipmipower_powercmd_queue (cmd_args.powercmd, &ics[i]);
        }
    }
  else
    /* in interactive mode errors should always go to the syslog */
    ierr_syslog (1);

#ifndef NDEBUG
  /* if debug set, send debug info to stderr too */
  ierr_cbuf (cmd_args.common.debug, ttyerr);
  
  /* on ierr_exit() dump cbuf data to appropriate places too */
  ierr_cbuf_dump_file_stream (cmd_args.common.debug, stderr);
#endif  /* !NDEBUG */

  ierr_file_descriptor (0, 0); /* now errors are done through the ttyerr */

  /* immediately send out discovery messages upon startup */
  ipmipower_ping_force_discovery_sweep ();

  _poll_loop ((cmd_args.powercmd != POWER_CMD_NONE) ? 1 : 0);

  exit_code = 0;
 cleanup:
  ipmipower_powercmd_cleanup ();
  _cleanup ();
  exit (0);
}
