/*****************************************************************************\
 *  $Id: ipmipower.c,v 1.34 2007-11-15 02:07:10 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
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
#endif /* STDC_HEADERS_H */
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

#include "ipmipower.h"
#include "ipmipower_config.h"
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
struct ipmipower_config *conf = NULL;

/* Array of all ipmi "connections" */
struct ipmipower_connection *ics = NULL;

/* Array of hostlists for short output */
int output_hostrange_flag = 0;
hostlist_t output_hostrange[MSG_TYPE_NUM_ENTRIES];

/* 
 * _security_initialization
 */
static void
_security_initialization(void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit rlim;

  if (getrlimit(RLIMIT_CORE, &rlim) < 0)
    err_exit("getrlimit: %s", strerror(errno));

  rlim.rlim_cur = 0;
  if (setrlimit(RLIMIT_CORE,&rlim) < 0)
    err_exit("setrlimit: %s", strerror(errno));
#endif /* NDEBUG */
  if (ipmi_rmcpplus_init() < 0)
    err_exit("ipmi_rmcpplus_init");
}

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

  if (conf->hosts != NULL) 
    {
      ics = ipmipower_connection_array_create(conf->hosts, conf->hosts_count);
      if (ics == NULL)
        exit(1);		/* error message output in the above call */
    }

  for (i = 0; i < MSG_TYPE_NUM_ENTRIES; i++) 
    {
      if ((output_hostrange[i] = hostlist_create(NULL)) == NULL)
        err_exit("hostlist_create() error");
    }
  
  /* errors should always go to atleast the syslog */
  err_syslog(1);

#ifndef NDEBUG
  if (strlen(conf->logfile)) 
    {
      conf->logfile_fd = open(conf->logfile, O_WRONLY | O_CREAT | O_APPEND,
                              S_IRUSR | S_IWUSR);
      if (conf->logfile_fd < 0)
        err_exit("error opening log file %s: %s", conf->logfile, strerror(errno));
    }

  /* if debug set, send debug info to stderr too */
  err_cbuf(conf->debug, ttyerr);

  /* on err_exit() dump cbuf data to appropriate places too */
  err_cbuf_dump_file_stream(conf->debug, stderr);
  err_cbuf_dump_file_descriptor(conf->log, conf->logfile_fd);

#else  /* !NDEBUG */
  err_cbuf(0, 0);
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
#ifndef NDEBUG
  if (conf->log)
    cbuf_peek_to_fd(ttyout, conf->logfile_fd, -1);
#endif /* NDEBUG */
  cbuf_read_to_fd(ttyout, STDOUT_FILENO, -1);
  cbuf_destroy(ttyout);
#ifndef NDEBUG
  if (conf->log)
    cbuf_peek_to_fd(ttyerr, conf->logfile_fd, -1);
#endif /* NDEBUG */
  cbuf_read_to_fd(ttyerr, STDERR_FILENO, -1);
  cbuf_destroy(ttyerr);

  ipmipower_connection_array_destroy(ics, conf->hosts_count);

  for (i = 0; i < MSG_TYPE_NUM_ENTRIES; i++)
    hostlist_destroy(output_hostrange[i]);

  hostlist_destroy(conf->hosts);

#ifndef NDEBUG
  close(conf->logfile_fd);
#endif /* NDEBUG */

#ifdef NDEBUG
  secure_free(conf, sizeof(struct ipmipower_config));
#else  /* !NDEBUG */
  free(conf);
#endif /* !NDEBUG */
}

static void 
_sendto(cbuf_t buf, int fd, struct sockaddr_in *destaddr) 
{
  int n, rv;
  char buffer[IPMI_PACKET_BUFLEN];

  if ((n = cbuf_read(buf, buffer, IPMI_PACKET_BUFLEN)) < 0)
    err_exit("_sendto(%d): cbuf_read: %s", fd, strerror(errno));
  if (n == IPMI_PACKET_BUFLEN)
    err_exit("_sendto: Buffer full");
    
  rv = ipmi_lan_sendto(fd, buffer, n, 0, (struct sockaddr *)destaddr,
                       sizeof(struct sockaddr_in));
  if (rv < 0)
    err_exit("_sendto: ipmi_lan_sendto %s", strerror(errno));
  if (rv != n)
    err_exit("_sendto: ipmi_lan_sendto rv=%d n=%d", rv, n);
  
  /* cbuf should be empty now */
  if (!cbuf_is_empty(buf))
    err_exit("_sendto: cbuf not empty");
}

static void 
_recvfrom(cbuf_t buf, int fd, struct sockaddr_in *srcaddr) 
{
  int n, rv, dropped = 0;
  char buffer[IPMI_PACKET_BUFLEN];
  struct sockaddr_in from;
  unsigned int fromlen = sizeof(struct sockaddr_in);
  
  rv = ipmi_lan_recvfrom(fd, buffer, IPMI_PACKET_BUFLEN, 0, 
			 (struct sockaddr *)&from, &fromlen);
  if (rv < 0)
    err_exit("_recvfrom: ipmi_lan_recvfrom: %s", strerror(errno));
  if (rv == 0)
    err_exit("_recvfrom: ipmi_lan_recvfrom: EOF");
  
  /* Don't store if this is packet is strange for some reason */
  if (from.sin_family != AF_INET 
      || from.sin_addr.s_addr != srcaddr->sin_addr.s_addr)
    return;
  
  /* cbuf should be empty, but if it isn't, empty it */
  if (!cbuf_is_empty(buf)) 
    {
      err_output("_recvfrom: cbuf not empty, draining");
      do {
        char tempbuf[IPMI_PACKET_BUFLEN];
        if (cbuf_read(buf, tempbuf, IPMI_PACKET_BUFLEN) < 0)
            err_exit("_recvfrom: cbuf_read: %s", strerror(errno));
      } while(!cbuf_is_empty(buf));
    }

  if ((n = cbuf_write(buf, buffer, rv, &dropped)) < 0)
    err_exit("_recvfrom(%d): cbuf_write: %s", fd, strerror(errno));
  if (n != rv)
    err_exit("_recvfrom: rv=%d n=%d", rv, n);
  if (dropped != 0)
    err_output("_recvfrom: read dropped %d bytes", dropped);
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
      if (non_interactive && num == 0)
        break;

      /* ping timeout is always set if conf->ping_interval_len > 0 */
      ipmipower_ping_process_pings(&ping_timeout);

      if (conf->ping_interval_len) 
        {
          if (powercmd_timeout == -1)
            timeout = ping_timeout;
          else 
            timeout = (ping_timeout < powercmd_timeout) ? 
              ping_timeout : powercmd_timeout;
        }
      else
        timeout = powercmd_timeout; 
      
      /* XXX: This poll() loop could be done far more elegantly and
       * efficiently without all this crazy indexing.  The best way
       * would be through some callback mechanism that would do the
       * callback based on POLLIN, POLLOUT, POLLERR, etc.
       *
       * In this function, we potentially
       * a) iterate in powercmd_process_pending
       * b) iterate in process_pings
       * c) iterate to create fds
       * d) iterate after the poll
       *
       * if done through a callback mechanism
       * a) no iterating in powercmd_process_pending, b/c callback
       * b) no iterating in process_pings, b/c callback
       * c) iterate to create fds (== iterations)
       * d) iterate after the poll (<= iterations b/c potentially less fds)
       *
       * So come back to this later.
       */

      /* Has the number of hosts changed? */
      if (nfds != (conf->hosts_count*2) + 3)
	{
	  /* The "*2" is for each host's two fds, one for ipmi
	   * (ipmi_fd) and one for rmcp (ping_fd).  The "+3" is for
	   * stdin, stdout, stderr.
	   */
	  nfds = (conf->hosts_count*2) + 3;   
	  Free(pfds);
	  pfds = (struct pollfd *)Malloc(nfds * sizeof(struct pollfd));
	}
      
      for (i = 0; i < conf->hosts_count; i++) 
        {
          pfds[i*2].fd = ics[i].ipmi_fd;
          pfds[i*2+1].fd = ics[i].ping_fd;
          pfds[i*2].events = pfds[i*2+1].events = 0;
          pfds[i*2].revents = pfds[i*2+1].revents = 0;
          
          pfds[i*2].events |= POLLIN;
          if (!cbuf_is_empty(ics[i].ipmi_out))
            pfds[i*2].events |= POLLOUT;
          
          if (!conf->ping_interval_len)
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
      
      for (i = 0; i < conf->hosts_count; i++) 
        {
          if (pfds[i*2].revents & POLLERR) 
            {
              err_output("_poll_loop: IPMI POLLERR, %s, %d", ics[i].hostname, 
                         ics[i].ipmi_fd);
              continue;
            }
          if (pfds[i*2].revents & POLLIN) 
            _recvfrom(ics[i].ipmi_in, ics[i].ipmi_fd, &(ics[i].destaddr));
          if (pfds[i*2].revents & POLLOUT)
            _sendto(ics[i].ipmi_out, ics[i].ipmi_fd, &(ics[i].destaddr));
          
          if (!conf->ping_interval_len)
            continue;
          
          if (pfds[i*2+1].revents & POLLERR) 
            {
              err_output("_poll_loop: PING POLLERR, %s, %d", ics[i].hostname, 
                         ics[i].ipmi_fd);
              continue;
            }
          if (pfds[i*2+1].revents & POLLIN) 
            _recvfrom(ics[i].ping_in, ics[i].ping_fd, &(ics[i].destaddr));
          if (pfds[i*2+1].revents & POLLOUT)
            _sendto(ics[i].ping_out, ics[i].ping_fd, &(ics[i].destaddr));
        }
      
      if (pfds[nfds-3].revents & POLLIN)
        {
          Cbuf_write_from_fd(ttyin, STDIN_FILENO);
#ifndef NDEBUG
          if (conf->log)
            Cbuf_peek_to_fd(ttyin, conf->logfile_fd, -1);
#endif /* NDEBUG */
        }
      if (!cbuf_is_empty(ttyout) && (pfds[nfds-2].revents & POLLOUT))
        {
#ifndef NDEBUG
          if (conf->log)
            Cbuf_peek_to_fd(ttyout, conf->logfile_fd, -1);
#endif /* NDEBUG */
          Cbuf_read_to_fd(ttyout, STDOUT_FILENO);
        }
      if (!cbuf_is_empty(ttyerr) && (pfds[nfds-1].revents & POLLOUT))
        {
#ifndef NDEBUG
          if (conf->log)
            Cbuf_peek_to_fd(ttyerr, conf->logfile_fd, -1);
#endif /* NDEBUG */
          Cbuf_read_to_fd(ttyerr, STDERR_FILENO);
        }
    }

  Free(pfds);
}

static void
_eliminate_nodes(void)
{
  if (conf->eliminate == IPMIPOWER_TRUE)
    {
      ipmidetect_t id = NULL;
      hostlist_iterator_t itr = NULL;
      char *host = NULL;

      if (!(id = ipmidetect_handle_create()))
        err_exit("ipmidetect_handle_create");
      
      if (ipmidetect_load_data(id,
                               NULL,
                               0,
                               0) < 0)
        {
          if (ipmidetect_errnum(id) == IPMIDETECT_ERR_CONNECT
              || ipmidetect_errnum(id) == IPMIDETECT_ERR_CONNECT_TIMEOUT)
            err_exit("Error connecting to ipmidetect daemon");
          err_exit("ipmidetect_load_data: %s\n", ipmidetect_errormsg(id));
        }
      
      if (!(itr = hostlist_iterator_create(conf->hosts)))
        err_exit("hostlist_iterator_create: %s", strerror(errno));
      
      while ((host = hostlist_next(itr)))
        {
          int ret;
          
          if ((ret = ipmidetect_is_node_detected(id, host)) < 0)
            {
              if (ipmidetect_errnum(id) == IPMIDETECT_ERR_NOTFOUND)
                err_exit("Node '%s' unrecognized by ipmidetect\n", host);
              err_exit("ipmidetect_is_node_detected: %s\n", ipmidetect_errormsg(id));
            }
          
          if (!ret)
            {
              hostlist_delete(conf->hosts, host);
              conf->hosts_count--;
            }
          
          free(host);
        }
    }
}

int 
main(int argc, char *argv[]) 
{
  /* Call before anything else */
  err_init(argv[0]);
  err_file_descriptor(1, STDERR_FILENO); /* initially errors goto stderr */
  
  _security_initialization();

  ipmipower_config_setup();
  assert(conf != NULL);

  /* Must be called before conffile_parse b/c --config option on command line*/
  ipmipower_config_cmdline_parse(argc, argv);
    
  ipmipower_config_conffile_parse(conf->configfile);
  ipmipower_config_check_values();

  _setup();

  ipmipower_powercmd_setup();
  
  /* If power command (i.e. --reset, --stat, etc.) is passed at
   * command line, put the power control commands in the pending
   * queue.
   */
  if (conf->powercmd != POWER_CMD_NONE) 
    {
      int i;

      /* Check for appropriate privilege first */
      if (conf->privilege_level_set_on_cmdline
          && conf->privilege_level == PRIVILEGE_LEVEL_USER 
          && POWER_CMD_REQUIRES_OPERATOR_PRIVILEGE_LEVEL(conf->powercmd))
        err_exit("power operation requires atleast operator privilege");

      _eliminate_nodes();

      for (i = 0; i < conf->hosts_count; i++) 
        {
          ipmipower_connection_clear(&ics[i]);
          ipmipower_powercmd_queue(conf->powercmd, &ics[i]);
        }
    }
  
  err_file_descriptor(0, 0); /* now errors are done through the ttyerr */

  /* immediately send out discovery messages upon startup */
  ipmipower_ping_force_discovery_sweep();

  _poll_loop((conf->powercmd != POWER_CMD_NONE) ? 1 : 0);
  
  ipmipower_powercmd_cleanup();
  _cleanup();
  exit(0);
}
