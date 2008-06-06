/*****************************************************************************\
 *  $Id: ipmidetectd_loop.c,v 1.8.6.1 2008-06-06 23:21:34 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-228523
 *
 *  This file is part of Ipmidetect, tools and libraries for detecting
 *  IPMI nodes in a cluster. For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmidetect is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmidetect is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmidetect.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
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
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/poll.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <signal.h>
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmidetectd.h"
#include "ipmidetectd_config.h"
#include "ipmidetectd_loop.h"
#include "debug.h"
#include "error.h"
#include "fd.h"
#include "hash.h"
#include "list.h"
#include "timeval.h"

#define IPMIDETECTD_BUFLEN           1024
#define IPMIDETECTD_NODES_PER_SOCKET 8
#define IPMIDETECTD_SERVER_BACKLOG   5

/* IPMI has a 6 bit sequence number */
#define IPMI_RQ_SEQ_MAX  0x3F

extern struct ipmidetectd_config conf;

static struct timeval ipmidetectd_next_send;

struct ipmidetectd_info
{
  char *hostname;
  int fd;
  struct sockaddr_in destaddr;
  unsigned int sequence_number;
  struct timeval last_received;
};

int *fds = NULL;
unsigned int fds_count = 0;
List nodes = NULL;
unsigned int nodes_count = 0;
hash_t nodes_index = NULL;
int server_fd = 0;

extern int h_errno;

static void
_fds_setup(void)
{
  struct sockaddr_in addr;
  int i;

  assert(!fds);
  assert(!fds_count);
  assert(!nodes_count);
  assert(!server_fd);

  nodes_count = hostlist_count(conf.hosts);
  fds_count = nodes_count/IPMIDETECTD_NODES_PER_SOCKET;
  if (nodes_count % IPMIDETECTD_NODES_PER_SOCKET)
    fds_count++;

  if (!(fds = (int *)malloc(fds_count * sizeof(int))))
    ERR_EXIT(("malloc: %s", strerror(errno)));

  for (i = 0; i < fds_count; i++)
    {
      if ((fds[i] = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
        ERR_EXIT(("socket: %s", strerror(errno)));

      memset(&addr, '\0', sizeof(struct sockaddr_in));
      addr.sin_family = AF_INET;
      addr.sin_port = htons(0);
      addr.sin_addr.s_addr = htonl(INADDR_ANY);

      if (bind(fds[i], (struct sockaddr *)&addr, sizeof(struct sockaddr_in)) < 0)
        ERR_EXIT(("bind: %s", strerror(errno)));
    }

  if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    ERR_EXIT(("socket: %s", strerror(errno)));
  
  memset(&addr, '\0', sizeof(struct sockaddr_in));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(conf.ipmidetectd_server_port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(server_fd, (struct sockaddr *)&addr, sizeof(struct sockaddr_in)) < 0)
    ERR_EXIT(("bind: %s", strerror(errno)));

  if (listen(server_fd, IPMIDETECTD_SERVER_BACKLOG) < 0)
    ERR_EXIT(("listen: %s", strerror(errno)));
}

static void
_nodes_setup(void)
{
  hostlist_iterator_t itr = NULL;
  char *host = NULL;
  int i = 0;

  assert(fds);
  assert(fds_count);
  assert(!nodes);
  assert(nodes_count);
  assert(!nodes_index);
  
  if (!(nodes = list_create((ListDelF)free)))
    ERR_EXIT(("list_create: %s", strerror(errno)));
  
  if (!(nodes_index = hash_create(nodes_count,
                                  (hash_key_f)hash_key_string,
                                  (hash_cmp_f)strcmp,
                                  NULL)))
    ERR_EXIT(("hash_create: %s", strerror(errno)));

  if (!(itr = hostlist_iterator_create(conf.hosts)))
    ERR_EXIT(("hostlist_iterator_create: %s", strerror(errno)));

  while ((host = hostlist_next(itr)))
    {
      struct ipmidetectd_info *info = NULL;
      struct hostent *h;
      char *tmpstr;
      char *ip;
      int len;

      if (!(info = (struct ipmidetectd_info *)malloc(sizeof(struct ipmidetectd_info))))
        ERR_EXIT(("malloc: %s", strerror(errno)));
      memset(info, '\0', sizeof(struct ipmidetectd_info));

      if (!(info->hostname = strdup(host)))
        ERR_EXIT(("strdup: %s", strerror(errno)));
      
      /* Use random number for starting sequence number to avoid probability of
       * duplicates and "hanging" BMC issue.
       */
      if ((len = ipmi_get_random((uint8_t *)&(info->sequence_number), 
                                 sizeof(info->sequence_number))) < 0)
        ERR_EXIT(("ipmi_get_random: %s", strerror(errno)));
      if (len != sizeof(info->sequence_number))
        ERR_EXIT(("ipmi_get_random: invalid len returned"));

      info->fd = fds[i/IPMIDETECTD_NODES_PER_SOCKET];
      
      if (!(h = gethostbyname(host)))
        {
#if HAVE_HSTRERROR
          ERR_EXIT(("gethostbyname: %s", hstrerror(h_errno)));
#else /* !HAVE_HSTRERROR */
          ERR_EXIT(("gethostbyname: h_errno = %d", h_errno));
#endif /* !HAVE_HSTRERROR */
        }
        
      info->destaddr.sin_family = AF_INET;
      info->destaddr.sin_addr = *((struct in_addr *)h->h_addr);
      info->destaddr.sin_port = htons(RMCP_PRIMARY_RMCP_PORT);
      free(host);
      
      if (!list_append(nodes, info))
        ERR_EXIT(("list_append: %s", strerror(errno)));

      if (!(tmpstr = inet_ntoa(info->destaddr.sin_addr)))
        ERR_EXIT(("inet_ntoa: %s", strerror(errno))); /* strerror? */

      if (!(ip = strdup(tmpstr)))
        ERR_EXIT(("strdup: %s", strerror(errno)));
        
      if (hash_find(nodes_index, ip))
        ERR_EXIT(("Duplicate host ip: %s", ip));

      if (!hash_insert(nodes_index, ip, info))
        ERR_EXIT(("hash_insert: %s", strerror(errno)));

      i++;
    }

  hostlist_iterator_destroy(itr);
}

static void
_ipmidetectd_setup(void)
{
  /* Initialize ipmidetectd_next_send to 0 so there is a sweep of pings in the beginning */
  memset(&ipmidetectd_next_send, '\0', sizeof(struct timeval));

  _fds_setup();
  _nodes_setup();

  /* Avoid sigpipe exiting during server writes */
  if (signal(SIGPIPE, SIG_IGN) == SIG_ERR)
    ERR_EXIT(("signal: %s", strerror(errno)));
}

static int
_ipmi_ping_build(struct ipmidetectd_info *info, char *buf, unsigned int buflen)
{
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_lan_session_hdr = NULL;
  fiid_obj_t obj_lan_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  int len;

  assert(info);
  assert(buf);
  assert(buflen);

  if (!(obj_rmcp_hdr = fiid_obj_create(tmpl_rmcp_hdr)))
    ERR_EXIT(("fiid_obj_create: %s", strerror(errno)));
  if (!(obj_lan_session_hdr = fiid_obj_create(tmpl_lan_session_hdr)))
    ERR_EXIT(("fiid_obj_create: %s", strerror(errno)));
  if (!(obj_lan_msg_hdr = fiid_obj_create(tmpl_lan_msg_hdr_rq)))
    ERR_EXIT(("fiid_obj_create: %s", strerror(errno)));
  if (!(obj_cmd = fiid_obj_create(tmpl_cmd_get_channel_authentication_capabilities_rq)))
    ERR_EXIT(("fiid_obj_create: %s", strerror(errno)));
  
  if (fill_rmcp_hdr_ipmi(obj_rmcp_hdr) < 0)
    ERR_EXIT(("fill_rmcp_hdr_ipmi: %s", strerror(errno)));
  
  if (fill_lan_session_hdr(IPMI_AUTHENTICATION_TYPE_NONE,
                           0,
                           0,
                           obj_lan_session_hdr) < 0)
    ERR_EXIT(("fill_lan_session_hdr: %s", strerror(errno)));

  if (fill_lan_msg_hdr(IPMI_NET_FN_APP_RQ, 
                       IPMI_BMC_IPMB_LUN_BMC,
                       info->sequence_number % (IPMI_RQ_SEQ_MAX+1), 
                       obj_lan_msg_hdr) < 0)
    ERR_EXIT(("fill_lan_msg_hdr: %s", strerror(errno)));

  if (fill_cmd_get_channel_authentication_capabilities(IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                                       IPMI_PRIVILEGE_LEVEL_USER,
                                                       obj_cmd) < 0)
    ERR_EXIT(("fill_cmd_get_channel_authentication_capabilities: %s", strerror(errno)));
  
  if ((len = assemble_ipmi_lan_pkt(obj_rmcp_hdr,
                                   obj_lan_session_hdr,
                                   obj_lan_msg_hdr,
                                   obj_cmd,
                                   NULL,
                                   0,
                                   (uint8_t *)buf,
                                   buflen)) < 0)
    ERR_EXIT(("assemble_ipmi_lan_pkt: %s", strerror(errno)));

#if 0
#ifndef NDEBUG
  if (conf.debug)
    {
      if (ipmi_dump_lan_packet(STDERR_FILENO,
                               info->hostname,
                               NULL,
                               (uint8_t *)buf,
                               len,
                               tmpl_lan_msg_hdr_rq,
                               tmpl_cmd_get_channel_authentication_capabilities_rq) < 0)
        ERR_EXIT(("ipmi_dump_lan_packet: %s", strerror(errno)));
    }
#endif /* NDEBUG */
#endif
  
  fiid_obj_destroy(obj_rmcp_hdr);
  fiid_obj_destroy(obj_lan_session_hdr);
  fiid_obj_destroy(obj_lan_msg_hdr);
  fiid_obj_destroy(obj_cmd);

  info->sequence_number++;
  return len;
}

static void
_ipmidetectd_send_pings(void)
{
  char buf[IPMIDETECTD_BUFLEN];
  int len;
  struct ipmidetectd_info *info;
  ListIterator itr;

  assert(nodes);
  assert(nodes_count);
  
  if (!(itr = list_iterator_create(nodes)))
    ERR_EXIT(("list_iterator_create: %s", strerror(errno)));

  while ((info = list_next(itr)))
    {
      memset(buf, '\0', IPMIDETECTD_BUFLEN);

      if ((len = _ipmi_ping_build(info, buf, IPMIDETECTD_BUFLEN)) < 0)
        ERR_EXIT(("_ipmi_ping_build: %s", strerror(errno)));

      if (sendto(info->fd, 
                 buf, 
                 len,
                 0, 
                 (struct sockaddr *)&(info->destaddr), 
                 sizeof(struct sockaddr_in)) < 0)
        ERR_EXIT(("sendto: %s", strerror(errno)));

#ifndef NDEBUG
      if (conf.debug)
        fprintf(stderr, "Ping Request to %s\n", info->hostname);
#endif /* NDEBUG */
    }

  list_iterator_destroy(itr);
}

static void
_setup_pfds(struct pollfd *pfds)
{
  int i;

  assert(pfds);

  for (i = 0; i < fds_count; i++)
    {
      pfds[i].fd = fds[i];
      pfds[i].events = POLLIN;
      pfds[i].revents = 0;
    }

  pfds[fds_count].fd = server_fd;
  pfds[fds_count].events = POLLIN;
  pfds[fds_count].revents = 0;
}

static void
_receive_ping(int fd)
{
  struct sockaddr_in from;
  struct ipmidetectd_info *info;
  char buf[IPMIDETECTD_BUFLEN];
  int len;
  socklen_t fromlen = sizeof(struct sockaddr_in);
  char *tmpstr;

  /* We're happy as long as we receive something.  We don't bother
   * checking sequence numbers or anything like that.
  */

  if ((len = recvfrom(fd, 
                      buf, 
                      IPMIDETECTD_BUFLEN, 
                      0, 
                      (struct sockaddr *)&from, 
                      &fromlen)) < 0)
    ERR_EXIT(("recvfrom: %s", strerror(errno)));

  if (!(tmpstr = inet_ntoa(from.sin_addr)))
    ERR_EXIT(("inet_ntoa: %s", strerror(errno))); /* strerror? */
  
  if ((info = hash_find(nodes_index, tmpstr)))
    {
      if (gettimeofday(&(info->last_received), NULL) < 0)
        ERR_EXIT(("gettimeofday: %s", strerror(errno)));

#ifndef NDEBUG
      if (conf.debug)
        fprintf(stderr, "Ping Reply from %s\n", info->hostname);
#endif /* NDEBUG */
    }
}

static void
_send_ping_data(void)
{
  ListIterator itr;
  struct sockaddr_in rhost;
  struct ipmidetectd_info *info;
  socklen_t rhost_len = sizeof(struct sockaddr_in);
  int rhost_fd;
  
  assert(nodes);
  assert(nodes_count);

  if ((rhost_fd = accept(server_fd, (struct sockaddr *)&rhost, &rhost_len)) < 0)
    ERR_EXIT(("accept: %s", strerror(errno)));
  
#ifndef NDEBUG
  if (conf.debug)
    fprintf(stderr, "Received ipmidetectd server request\n");
#endif /* NDEBUG */
  
  if (!(itr = list_iterator_create(nodes)))
    ERR_EXIT(("list_iterator_create: %s", strerror(errno)));

  while ((info = list_next(itr)))
    {
      char buf[IPMIDETECTD_BUFLEN];
      int len, n;

      len = snprintf(buf, IPMIDETECTD_BUFLEN, "%s %lu\n", info->hostname, info->last_received.tv_sec);
      if (len >= IPMIDETECTD_BUFLEN)
        ERR_EXIT(("len=%d", len));

      if ((n = fd_write_n(rhost_fd, buf, len)) < 0)
        {
          if (errno == EPIPE)
            break;
          else
            ERR_EXIT(("fd_write_n: %s", strerror(errno)));
        }

      if (n != len)
        ERR_EXIT(("fd_write_n: n=%d len=%d", n, len));
    }

  list_iterator_destroy(itr);
  close(rhost_fd);
}

void 
ipmidetectd_loop(void)
{
  struct pollfd *pfds = NULL;
  int i;

  _ipmidetectd_setup();

  assert(nodes_count);

  /* +1 fd for the server fd */
  if (!(pfds = (struct pollfd *)malloc((fds_count + 1)*sizeof(struct pollfd))))
    ERR_EXIT(("malloc: %s", strerror(errno)));

  while (1)
    {
      struct timeval now, timeout;
      unsigned int timeout_ms;
      int num;

      if (gettimeofday(&now, NULL) < 0)
        ERR_EXIT(("gettimeofday: %s", strerror(errno)));

      if (timeval_gt(&now, &ipmidetectd_next_send))
        {
          _ipmidetectd_send_pings();

          if (gettimeofday(&now, NULL) < 0)
            ERR_EXIT(("gettimeofday: %s", strerror(errno)));

          timeval_add_ms(&now, conf.ipmiping_period, &ipmidetectd_next_send);
        }

      _setup_pfds(pfds);

      timeval_sub(&ipmidetectd_next_send, &now, &timeout);
      timeval_millisecond_calc(&timeout, &timeout_ms);

      if ((num = poll(pfds, fds_count + 1, timeout_ms)) < 0)
        ERR_EXIT(("poll: %s", strerror(errno)));
      
      if (num)
        {
          for (i = 0; i < fds_count; i++)
            {
              if (pfds[i].revents & POLLIN)
                _receive_ping(fds[i]);
            }

          if (pfds[fds_count].revents & POLLIN)
            _send_ping_data();
        }
    }
}
