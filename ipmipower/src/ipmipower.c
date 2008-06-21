/*****************************************************************************\
 *  $Id: ipmipower.c,v 1.70 2008-06-21 14:34:12 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
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
#include <errno.h>

#include "freeipmi-portability.h"
#include "tool-common.h"

#include "ipmipower.h"
#include "ipmipower_argp.h"
#include "ipmipower_connection.h"
#include "ipmipower_powercmd.h"
#include "ipmipower_prompt.h"
#include "ipmipower_ping.h"
#include "ipmipower_util.h"
#include "ipmipower_wrappers.h"

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
static void 
_setup(void) 
{
  int i;
  struct rlimit rlim;
  
  srand(Time(NULL));

  /* Make best effort to increase file descriptor limit, if it fails
   * for any reason, don't worry about it, its no big deal.
   */
  
  if (getrlimit(RLIMIT_NOFILE, &rlim) == 0)
    {
      rlim.rlim_cur = rlim.rlim_max;
      setrlimit(RLIMIT_NOFILE,&rlim);
    }

  /* Create TTY bufs */
  ttyin  = Cbuf_create(IPMIPOWER_MIN_TTY_BUF, IPMIPOWER_MAX_TTY_BUF);
  ttyout = Cbuf_create(IPMIPOWER_MIN_TTY_BUF, IPMIPOWER_MAX_TTY_BUF);
  ttyerr = Cbuf_create(IPMIPOWER_MIN_TTY_BUF, IPMIPOWER_MAX_TTY_BUF);

  for (i = 0; i < MSG_TYPE_NUM_ENTRIES; i++) 
    {
      if (!(output_hostrange[i] = hostlist_create(NULL)))
        ierr_exit("hostlist_create() error");
    }
  
  /* in interactive mode errors should always go to atleast the syslog */
  ierr_syslog(1);

#ifndef NDEBUG
  /* if debug set, send debug info to stderr too */
  ierr_cbuf(cmd_args.common.debug, ttyerr);

  /* on ierr_exit() dump cbuf data to appropriate places too */
  ierr_cbuf_dump_file_stream(cmd_args.common.debug, stderr);
#else  /* !NDEBUG */
  ierr_cbuf(0, 0);
#endif /* !NDEBUG */
}

/* _cleanup
 * - Cleanup dynamic memory allocated in this program
 */
static void 
_cleanup(void) 
{
  int i;

  cbuf_destroy(ttyin);
  
  /* Flush before destroying. */
  cbuf_read_to_fd(ttyout, STDOUT_FILENO, -1);
  cbuf_destroy(ttyout);
  cbuf_read_to_fd(ttyerr, STDERR_FILENO, -1);
  cbuf_destroy(ttyerr);

  ipmipower_connection_array_destroy(ics, ics_len);

  for (i = 0; i < MSG_TYPE_NUM_ENTRIES; i++)
    hostlist_destroy(output_hostrange[i]);
}

static void 
_sendto(cbuf_t buf, int fd, struct sockaddr_in *destaddr) 
{
  int n, rv;
  char buffer[IPMIPOWER_PACKET_BUFLEN];

  if ((n = cbuf_read(buf, buffer, IPMIPOWER_PACKET_BUFLEN)) < 0)
    ierr_exit("_sendto(%d): cbuf_read: %s", fd, strerror(errno));
  if (n == IPMIPOWER_PACKET_BUFLEN)
    ierr_exit("_sendto: Buffer full");
    
  rv = ipmi_lan_sendto(fd, buffer, n, 0, (struct sockaddr *)destaddr,
                       sizeof(struct sockaddr_in));
  if (rv < 0)
    ierr_exit("_sendto: ipmi_lan_sendto %s", strerror(errno));
  if (rv != n)
    ierr_exit("_sendto: ipmi_lan_sendto rv=%d n=%d", rv, n);
  
  /* cbuf should be empty now */
  if (!cbuf_is_empty(buf))
    ierr_exit("_sendto: cbuf not empty");
}

static void 
_recvfrom(cbuf_t buf, int fd, struct sockaddr_in *srcaddr) 
{
  int n, rv, dropped = 0;
  char buffer[IPMIPOWER_PACKET_BUFLEN];
  struct sockaddr_in from;
  unsigned int fromlen = sizeof(struct sockaddr_in);
  
  rv = ipmi_lan_recvfrom(fd, buffer, IPMIPOWER_PACKET_BUFLEN, 0, 
			 (struct sockaddr *)&from, &fromlen);
  if (rv < 0)
    ierr_exit("_recvfrom: ipmi_lan_recvfrom: %s", strerror(errno));
  if (!rv)
    ierr_exit("_recvfrom: ipmi_lan_recvfrom: EOF");
  
  /* Don't store if this is packet is strange for some reason */
  if (from.sin_family != AF_INET 
      || from.sin_addr.s_addr != srcaddr->sin_addr.s_addr)
    return;
  
  /* cbuf should be empty, but if it isn't, empty it */
  if (!cbuf_is_empty(buf)) 
    {
      ierr_dbg("_recvfrom: cbuf not empty, draining");
      do 
        {
          char tempbuf[IPMIPOWER_PACKET_BUFLEN];
          if (cbuf_read(buf, tempbuf, IPMIPOWER_PACKET_BUFLEN) < 0)
            ierr_exit("_recvfrom: cbuf_read: %s", strerror(errno));
        } while(!cbuf_is_empty(buf));
    }

  if ((n = cbuf_write(buf, buffer, rv, &dropped)) < 0)
    ierr_exit("_recvfrom(%d): cbuf_write: %s", fd, strerror(errno));
  if (n != rv)
    ierr_exit("_recvfrom: rv=%d n=%d", rv, n);
  if (dropped)
    ierr_dbg("_recvfrom: read dropped %d bytes", dropped);
}

/* _poll_loop
 * - poll on all descriptors
 */
static void 
_poll_loop(int non_interactive) 
{
  int nfds = 0;
  struct pollfd *pfds = NULL;

  while (non_interactive || ipmipower_prompt_process_cmdline()) 
    {
      int i, num, timeout;
      int powercmd_timeout = -1;
      int ping_timeout = -1;
      
      /* If there are no pending commands before this call,
       * powercmd_timeout will not be set, leaving it at -1
       */
      num = ipmipower_powercmd_process_pending(&powercmd_timeout);
      if (non_interactive && !num)
        break;

      /* ping timeout is always set if cmd_args.ping_interval > 0 */
      ipmipower_ping_process_pings(&ping_timeout);

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
	  Free(pfds);
	  pfds = (struct pollfd *)Malloc(nfds * sizeof(struct pollfd));
	}
      
      for (i = 0; i < ics_len; i++) 
        {
          pfds[i*2].fd = ics[i].ipmi_fd;
          pfds[i*2+1].fd = ics[i].ping_fd;
          pfds[i*2].events = pfds[i*2+1].events = 0;
          pfds[i*2].revents = pfds[i*2+1].revents = 0;
          
          pfds[i*2].events |= POLLIN;
          if (!cbuf_is_empty(ics[i].ipmi_out))
            pfds[i*2].events |= POLLOUT;
          
          if (!cmd_args.ping_interval)
            continue;
          
          pfds[i*2+1].events |= POLLIN;
          if (!cbuf_is_empty(ics[i].ping_out))
            pfds[i*2+1].events |= POLLOUT;
        }
      
      pfds[nfds-3].fd = STDIN_FILENO;
      pfds[nfds-3].events = POLLIN;
      pfds[nfds-3].revents = 0;
      pfds[nfds-2].fd = STDOUT_FILENO;
      if (!cbuf_is_empty(ttyout))
        pfds[nfds-2].events = POLLOUT;
      else
        pfds[nfds-2].events = 0;
      pfds[nfds-2].revents = 0;
      pfds[nfds-1].fd = STDERR_FILENO;
      if (!cbuf_is_empty(ttyerr))
        pfds[nfds-1].events = POLLOUT;
      else
        pfds[nfds-1].events = 0;
      pfds[nfds-1].revents = 0;

      Poll(pfds, nfds, timeout);
      
      for (i = 0; i < ics_len; i++) 
        {
          if (pfds[i*2].revents & POLLERR) 
            {
              ierr_dbg("_poll_loop: IPMI POLLERR, %s, %d", ics[i].hostname, 
                       ics[i].ipmi_fd);
              continue;
            }
          if (pfds[i*2].revents & POLLIN) 
            _recvfrom(ics[i].ipmi_in, ics[i].ipmi_fd, &(ics[i].destaddr));
          if (pfds[i*2].revents & POLLOUT)
            _sendto(ics[i].ipmi_out, ics[i].ipmi_fd, &(ics[i].destaddr));
          
          if (!cmd_args.ping_interval)
            continue;
          
          if (pfds[i*2+1].revents & POLLERR) 
            {
              ierr_dbg("_poll_loop: PING POLLERR, %s, %d", ics[i].hostname, 
                       ics[i].ipmi_fd);
              continue;
            }
          if (pfds[i*2+1].revents & POLLIN) 
            _recvfrom(ics[i].ping_in, ics[i].ping_fd, &(ics[i].destaddr));
          if (pfds[i*2+1].revents & POLLOUT)
            _sendto(ics[i].ping_out, ics[i].ping_fd, &(ics[i].destaddr));
        }
      
      if (pfds[nfds-3].revents & POLLIN)
        Cbuf_write_from_fd(ttyin, STDIN_FILENO);
      if (!cbuf_is_empty(ttyout) && (pfds[nfds-2].revents & POLLOUT))
        Cbuf_read_to_fd(ttyout, STDOUT_FILENO);
      if (!cbuf_is_empty(ttyerr) && (pfds[nfds-1].revents & POLLOUT))
        Cbuf_read_to_fd(ttyerr, STDERR_FILENO);
    }

  Free(pfds);
}

static void
_eliminate_nodes(void)
{
  if (cmd_args.hostrange.eliminate)
    {
      ipmidetect_t id = NULL;
      int i;

      if (!(id = ipmidetect_handle_create()))
        ierr_exit("ipmidetect_handle_create");
      
      if (ipmidetect_load_data(id,
                               NULL,
                               0,
                               0) < 0)
        {
          if (ipmidetect_errnum(id) == IPMIDETECT_ERR_CONNECT
              || ipmidetect_errnum(id) == IPMIDETECT_ERR_CONNECT_TIMEOUT)
            ierr_exit("Error connecting to ipmidetect daemon");
          ierr_exit("ipmidetect_load_data: %s", ipmidetect_errormsg(id));
        }
      
      for (i = 0; i < ics_len; i++)
        {
          int ret;
          
          if ((ret = ipmidetect_is_node_detected(id, ics[i].hostname)) < 0)
            {
              if (ipmidetect_errnum(id) == IPMIDETECT_ERR_NOTFOUND)
                ierr_exit("Node '%s' unrecognized by ipmidetect", ics[i].hostname);
              ierr_exit("ipmidetect_is_node_detected: %s", ipmidetect_errormsg(id));
            }

          if (!ret)
            ics[i].skip++;
        }
      
      ipmidetect_handle_destroy(id);
    }
}

int 
main(int argc, char *argv[]) 
{
  /* Call before anything else */
  ierr_init(argv[0]);
  ierr_file_descriptor(1, STDERR_FILENO); /* initially errors goto stderr */
  
  ipmi_disable_coredump();

  if (ipmi_rmcpplus_init() < 0)
    ierr_exit("ipmi_rmcpplus_init");

  ipmipower_argp_parse (argc, argv, &cmd_args);

  _setup();

  ipmipower_powercmd_setup();
  
  if (cmd_args.common.hostname) 
    {
      unsigned int len = 0;
      if (!(ics = ipmipower_connection_array_create(cmd_args.common.hostname, &len)))
        {
          /* dump error outputs here, most notably invalid hostname output */
          if (cbuf_read_to_fd(ttyout, STDOUT_FILENO, -1) > 0)
            exit(1);
          ierr_exit("ipmipower_connection_array_create: %s", strerror(errno));
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
          && POWER_CMD_REQUIRES_OPERATOR_PRIVILEGE_LEVEL(cmd_args.powercmd))
        ierr_exit("power operation requires atleast operator privilege");

      _eliminate_nodes();

      for (i = 0; i < ics_len; i++) 
        {
          if (ics[i].skip)
            continue;
          ipmipower_powercmd_queue(cmd_args.powercmd, &ics[i]);
        }
    }
  
  ierr_file_descriptor(0, 0); /* now errors are done through the ttyerr */

  /* immediately send out discovery messages upon startup */
  ipmipower_ping_force_discovery_sweep();

  _poll_loop((cmd_args.powercmd != POWER_CMD_NONE) ? 1 : 0);
  
  ipmipower_powercmd_cleanup();
  _cleanup();
  exit(0);
}
