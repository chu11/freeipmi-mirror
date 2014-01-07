/*****************************************************************************\
 *  $Id: ipmiconsole_engine.c,v 1.97 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiconsole is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiconsole is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/types.h>
#include <sys/poll.h>
#include <signal.h>
#include <limits.h>
#include <assert.h>
#include <errno.h>
#include <freeipmi/freeipmi.h>

#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

#include "ipmiconsole_ctx.h"
#include "ipmiconsole_engine.h"
#include "ipmiconsole_debug.h"
#include "ipmiconsole_garbage_collector.h"
#include "ipmiconsole_processing.h"
#include "ipmiconsole_util.h"
#include "scbuf.h"

#include "freeipmi-portability.h"
#include "list.h"
#include "secure.h"

/*
 * Locking notes:
 *
 * when is_count mutex is locked - thread_count_mutex can be locked, not vice versa
 * when is_count mutex is locked - teardown_mutex can be locked, not vice versa
 * when thread_count mutex is locked - ctxs_mutex can be locked, not vice versa
 */
static int console_engine_is_setup = 0;
static pthread_mutex_t console_engine_is_setup_mutex = PTHREAD_MUTEX_INITIALIZER;

static unsigned int console_engine_thread_count = 0;
static pthread_mutex_t console_engine_thread_count_mutex = PTHREAD_MUTEX_INITIALIZER;

static int console_engine_teardown = 0;
static int console_engine_teardown_immediate = 0;
static pthread_mutex_t console_engine_teardown_mutex = PTHREAD_MUTEX_INITIALIZER;

static List console_engine_ctxs[IPMICONSOLE_THREAD_COUNT_MAX];
static unsigned int console_engine_ctxs_count[IPMICONSOLE_THREAD_COUNT_MAX];
static pthread_mutex_t console_engine_ctxs_mutex[IPMICONSOLE_THREAD_COUNT_MAX];

/* In the core engine code, the poll() may sit for a large number of
 * seconds, waiting for the next event to happen.  In the meantime, a
 * user may have submitted a new context or wants to close the engine.
 * The poll() doesn't know this and will sit until it times out,
 * letting the user sit and wait for the engine loop to "come around
 * again" and start processing.  This pipe can be used to "interrupt"
 * the poll() when the user wants to get things moving a little
 * faster.
 */
static int console_engine_ctxs_notifier[IPMICONSOLE_THREAD_COUNT_MAX][2];
static unsigned int console_engine_ctxs_notifier_num = 0;

/*
 * The engine is capable of "being finished" with a context before the
 * user has called ipmiconsole_ctx_destroy().  So we need to stick the
 * context somewhere and garbage collect the memory back later.
 *
 * The garbage collector notifier is similar to the engine ctxs
 * notifier above, although the garbage collector notifier will only
 * be used to tell it to exit, not anything else.
 */
List console_engine_ctxs_to_destroy = NULL;
pthread_mutex_t console_engine_ctxs_to_destroy_mutex = PTHREAD_MUTEX_INITIALIZER;
int garbage_collector_notifier[2];

extern int garbage_collector_active;
extern pthread_mutex_t garbage_collector_active_mutex;
extern pthread_cond_t garbage_collector_active_cond;

/* See comments below in _poll_setup(). */
static int dummy_fd = -1;

struct _ipmiconsole_poll_data {
  struct pollfd *pfds;
  ipmiconsole_ctx_t *pfds_ctxs;
  unsigned int ctxs_len;
  unsigned int pfds_index;
};

#define IPMICONSOLE_SPIN_WAIT_TIME 250000

#define IPMICONSOLE_PIPE_BUFLEN 1024

static int
_ipmiconsole_garbage_collector_create (void)
{
  pthread_t thread;
  pthread_attr_t attr;
  int perr, rv = -1;

  assert (!console_engine_is_setup);

  if ((perr = pthread_attr_init (&attr)))
    {
      IPMICONSOLE_DEBUG (("pthread_attr_init: %s", strerror (perr)));
      errno = perr;
      goto cleanup;
    }

  if ((perr = pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED)))
    {
      IPMICONSOLE_DEBUG (("pthread_attr_setdetachstate: %s", strerror (perr)));
      errno = perr;
      goto cleanup;
    }

  if ((perr = pthread_create (&thread, &attr, ipmiconsole_garbage_collector, NULL)))
    {
      IPMICONSOLE_DEBUG (("pthread_create: %s", strerror (perr)));
      errno = perr;
      goto cleanup;
    }

  /* Who cares if this fails */
  if ((perr = pthread_attr_destroy (&attr)))
    IPMICONSOLE_DEBUG (("pthread_attr_destroy: %s", strerror (perr)));

  if ((perr = pthread_mutex_lock (&garbage_collector_active_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      errno = perr;
      goto cleanup;
    }

  while (!garbage_collector_active)
    {
      if ((perr = pthread_cond_wait (&garbage_collector_active_cond,
				     &garbage_collector_active_mutex)))
	{
	  IPMICONSOLE_DEBUG (("pthread_cond_wait: %s", strerror (perr)));
	  errno = perr;
	  goto cleanup;
	}
    }

  if ((perr = pthread_mutex_unlock (&garbage_collector_active_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      errno = perr;
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

/* Notes: One of the reason we do not create the threads in this
 * function, is that it would make it more difficult to properly
 * "cleanup" on an error.  We wouldn't know how many threads were
 * created, flags for setup completion may not be set yet, etc.
 *
 * Therefore ipmiconsole_engine_thread_create() is done outside of
 * this function and is done elsewhere.
 */
int
ipmiconsole_engine_setup (unsigned int thread_count)
{
  unsigned int i;
  int perr;

  assert (!console_engine_thread_count);
  assert (thread_count && thread_count <= IPMICONSOLE_THREAD_COUNT_MAX);

  if ((perr = pthread_mutex_lock (&console_engine_is_setup_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      errno = perr;
      return (-1);
    }

  memset (console_engine_ctxs, '\0', IPMICONSOLE_THREAD_COUNT_MAX * sizeof (List));
  memset (console_engine_ctxs_count, '\0', IPMICONSOLE_THREAD_COUNT_MAX * sizeof (unsigned int));
  memset (console_engine_ctxs_mutex, '\0', IPMICONSOLE_THREAD_COUNT_MAX * sizeof (pthread_mutex_t));
  for (i = 0; i < IPMICONSOLE_THREAD_COUNT_MAX; i++)
    {
      console_engine_ctxs_notifier[i][0] = -1;
      console_engine_ctxs_notifier[i][1] = -1;
    }
  garbage_collector_notifier[0] = -1;
  garbage_collector_notifier[1] = -1;

  if (ipmi_rmcpplus_init () < 0)
    {
      if (errno == EPERM)
        IPMICONSOLE_DEBUG (("ipmi_rmcpplus_init: incompatible crypto library"));
      else
        IPMICONSOLE_DEBUG (("ipmi_rmcpplus_init: %s", strerror (errno)));
      goto cleanup;
    }

  for (i = 0; i < IPMICONSOLE_THREAD_COUNT_MAX; i++)
    {
      if (!(console_engine_ctxs[i] = list_create ((ListDelF)ipmiconsole_ctx_connection_cleanup_session_submitted)))
        {
          IPMICONSOLE_DEBUG (("list_create: %s", strerror (errno)));
          goto cleanup;
        }
      console_engine_ctxs_count[i] = 0;
      if ((perr = pthread_mutex_init (&console_engine_ctxs_mutex[i], NULL)) != 0)
        {
          IPMICONSOLE_DEBUG (("pthread_mutex_init: %s", strerror (perr)));
          goto cleanup;
        }
    }

  /* Don't create fds for all ctxs_notifier to limit fd creation */
  console_engine_ctxs_notifier_num = thread_count;
  for (i = 0; i < console_engine_ctxs_notifier_num; i++)
    {
      if (pipe (console_engine_ctxs_notifier[i]) < 0)
        {
          IPMICONSOLE_DEBUG (("pipe: %s", strerror (errno)));
          goto cleanup;
        }

      if (ipmiconsole_set_closeonexec (NULL, console_engine_ctxs_notifier[i][0]) < 0)
        {
          IPMICONSOLE_DEBUG (("closeonexec error"));
          goto cleanup;
        }

      if (ipmiconsole_set_closeonexec (NULL, console_engine_ctxs_notifier[i][1]) < 0)
        {
          IPMICONSOLE_DEBUG (("closeonexec error"));
          goto cleanup;
        }
    }

  if (pipe (garbage_collector_notifier) < 0)
    {
      IPMICONSOLE_DEBUG (("pipe: %s", strerror (errno)));
      goto cleanup;
    }

  if (ipmiconsole_set_closeonexec (NULL, garbage_collector_notifier[0]) < 0)
    {
      IPMICONSOLE_DEBUG (("closeonexec error"));
      goto cleanup;
    }

  if (ipmiconsole_set_closeonexec (NULL, garbage_collector_notifier[1]) < 0)
    {
      IPMICONSOLE_DEBUG (("closeonexec error"));
      goto cleanup;
    }

  if (!(console_engine_ctxs_to_destroy = list_create ((ListDelF)ipmiconsole_ctx_list_cleanup)))
    {
      IPMICONSOLE_DEBUG (("list_create: %s", strerror (errno)));
      goto cleanup;
    }

  if ((dummy_fd = socket (AF_INET, SOCK_STREAM, 0)) < 0)
    {
      IPMICONSOLE_DEBUG (("socket: %s", strerror (errno)));
      goto cleanup;
    }

  if (_ipmiconsole_garbage_collector_create () < 0)
    goto cleanup;

  console_engine_is_setup++;
  console_engine_teardown = 0;
  console_engine_teardown_immediate = 0;

  if ((perr = pthread_mutex_unlock (&console_engine_is_setup_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      errno = perr;
      goto cleanup;
    }

  return (0);

 cleanup:
  for (i = 0; i < IPMICONSOLE_THREAD_COUNT_MAX; i++)
    {
      if (console_engine_ctxs[i])
        {
          list_destroy (console_engine_ctxs[i]);
          pthread_mutex_destroy (&console_engine_ctxs_mutex[i]);
        }
      console_engine_ctxs[i] = NULL;
      /* ignore potential error, cleanup path */
      close (console_engine_ctxs_notifier[i][0]);
      /* ignore potential error, cleanup path */
      close (console_engine_ctxs_notifier[i][1]);
    }
  if (console_engine_ctxs_to_destroy)
    list_destroy (console_engine_ctxs_to_destroy);
  console_engine_ctxs_to_destroy = NULL;
  garbage_collector_notifier[0] = -1;
  garbage_collector_notifier[1] = -1;
  /* ignore potential error, cleanup path */
  close (dummy_fd);
  dummy_fd = -1;

  if ((perr = pthread_mutex_unlock (&console_engine_is_setup_mutex)))
    IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));

  return (-1);
}

int
ipmiconsole_engine_is_setup (void)
{
  int is_setup, perr;

  if ((perr = pthread_mutex_lock (&console_engine_is_setup_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      return (-1);
    }

  is_setup = console_engine_is_setup;

  if ((perr = pthread_mutex_unlock (&console_engine_is_setup_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      return (-1);
    }

  return (is_setup);
}

int
ipmiconsole_engine_thread_count (void)
{
  int thread_count, perr;

  if ((perr = pthread_mutex_lock (&console_engine_thread_count_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      return (-1);
    }

  if (console_engine_thread_count > INT_MAX)
    thread_count = INT_MAX;
  else
    thread_count = console_engine_thread_count;

  if ((perr = pthread_mutex_unlock (&console_engine_thread_count_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      return (-1);
    }

  return (thread_count);
}

static int
_teardown_initiate (void *x, void *arg)
{
  ipmiconsole_ctx_t c;

  assert (x);

  c = (ipmiconsole_ctx_t)x;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);

  if (!c->session.close_session_flag)
    c->session.close_session_flag++;

  return (0);
}

static int
_poll_setup (void *x, void *arg)
{
  ipmiconsole_ctx_t c;
  struct _ipmiconsole_poll_data *poll_data;

  assert (x);
  assert (arg);

  c = (ipmiconsole_ctx_t)x;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);

  poll_data = (struct _ipmiconsole_poll_data *)arg;

  poll_data->pfds[poll_data->pfds_index*3].fd = c->connection.ipmi_fd;
  poll_data->pfds[poll_data->pfds_index*3].events = 0;
  poll_data->pfds[poll_data->pfds_index*3].revents = 0;
  poll_data->pfds[poll_data->pfds_index*3].events |= POLLIN;
  if (!scbuf_is_empty (c->connection.ipmi_to_bmc))
    poll_data->pfds[poll_data->pfds_index*3].events |= POLLOUT;

  /* If the session is being torn down, don't bother settings flags on
   * these fds.  However, to avoid spinning due to an invalid fd or a
   * closed fd (i.e. get a POLLINVAL or POLLHUP), use the dummy_fd.
   */
  if (!c->session.close_session_flag)
    {
      poll_data->pfds[poll_data->pfds_index*3 + 1].fd = c->connection.asynccomm[0];
      poll_data->pfds[poll_data->pfds_index*3 + 1].events = 0;
      poll_data->pfds[poll_data->pfds_index*3 + 1].revents = 0;
      poll_data->pfds[poll_data->pfds_index*3 + 1].events |= POLLIN;

      poll_data->pfds[poll_data->pfds_index*3 + 2].fd = c->connection.ipmiconsole_fd;
      poll_data->pfds[poll_data->pfds_index*3 + 2].events = 0;
      poll_data->pfds[poll_data->pfds_index*3 + 2].revents = 0;
      poll_data->pfds[poll_data->pfds_index*3 + 2].events |= POLLIN;
      if (!scbuf_is_empty (c->connection.console_bmc_to_remote_console))
        poll_data->pfds[poll_data->pfds_index*3 + 2].events |= POLLOUT;
    }
  else
    {
      poll_data->pfds[poll_data->pfds_index*3 + 1].fd = dummy_fd;
      poll_data->pfds[poll_data->pfds_index*3 + 1].events = 0;
      poll_data->pfds[poll_data->pfds_index*3 + 1].revents = 0;

      poll_data->pfds[poll_data->pfds_index*3 + 2].fd = dummy_fd;
      poll_data->pfds[poll_data->pfds_index*3 + 2].events = 0;
      poll_data->pfds[poll_data->pfds_index*3 + 2].revents = 0;
    }

  poll_data->pfds_ctxs[poll_data->pfds_index] = c;

  poll_data->pfds_index++;
  return (0);
}

/*
 * Return 0 on success
 * Return -1 on fatal error
 */
static int
_ipmi_recvfrom (ipmiconsole_ctx_t c)
{
  char buffer[IPMICONSOLE_PACKET_BUFLEN];
  struct sockaddr_in from;
  unsigned int fromlen = sizeof (struct sockaddr_in);
  ssize_t len;
  int n, dropped = 0;
  int secure_malloc_flag;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);

  secure_malloc_flag = (c->config.engine_flags & IPMICONSOLE_ENGINE_LOCK_MEMORY) ? 1 : 0;

  do
    {
      /* For receive side, ipmi_lan_recvfrom and
       * ipmi_rmcpplus_recvfrom are identical.  So we just use
       * ipmi_lan_recvfrom for both.
       *
       * In event of future change, should use util functions
       * ipmi_is_ipmi_1_5_packet or ipmi_is_ipmi_2_0_packet
       * appropriately.
       */
      len = ipmi_lan_recvfrom (c->connection.ipmi_fd,
                               buffer,
                               IPMICONSOLE_PACKET_BUFLEN,
                               0,
                               (struct sockaddr *)&from,
                               &fromlen);
    } while (len < 0 && errno == EINTR);

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
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_lan_recvfrom: connection refused: %s", strerror (errno)));
      return (0);
    }

  if (len < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_lan_recvfrom: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      return (-1);
    }

  if (!len)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_lan_recvfrom: no data", strerror (errno)));
      /* Note: Not a fatal error, just return*/
      return (0);
    }

  /* Sanity Check */
  if (from.sin_family != AF_INET
      || from.sin_addr.s_addr != c->session.addr.sin_addr.s_addr)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("received from invalid address"));
      /* Note: Not a fatal error, just return */
      return (0);
    }

  /* Empty the scbuf if it's not empty */
  if (!scbuf_is_empty (c->connection.ipmi_from_bmc))
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_from_bmc not empty, draining"));
      do {
        char tempbuf[IPMICONSOLE_PACKET_BUFLEN];
        if (scbuf_read (c->connection.ipmi_from_bmc, tempbuf, IPMICONSOLE_PACKET_BUFLEN) < 0)
          {
            IPMICONSOLE_CTX_DEBUG (c, ("scbuf_read: %s", strerror (errno)));
            break;
          }
      } while(!scbuf_is_empty (c->connection.ipmi_from_bmc));
    }

  if ((n = scbuf_write (c->connection.ipmi_from_bmc, buffer, len, &dropped, secure_malloc_flag)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("scbuf_write: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (n != len)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("scbuf_write: invalid bytes written; n=%d; len=%d", n, len));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (dropped)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("scbuf_write: dropped data: dropped=%d", dropped));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  return (0);
}

/*
 * Return 0 on success
 * Return -1 on fatal error
 */
static int
_ipmi_sendto (ipmiconsole_ctx_t c)
{
  char buffer[IPMICONSOLE_PACKET_BUFLEN];
  ssize_t len;
  int n;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);

  if ((n = scbuf_read (c->connection.ipmi_to_bmc, buffer, IPMICONSOLE_PACKET_BUFLEN)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("scbuf_read: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (ipmi_is_ipmi_1_5_packet (buffer, n))
    {
      do
	{
	  len = ipmi_lan_sendto (c->connection.ipmi_fd,
				 buffer,
				 n,
				 0,
				 (struct sockaddr *)&(c->session.addr),
				 sizeof (struct sockaddr_in));
	} while (len < 0 && errno == EINTR);
    }
  else
    {
      do
	{
	  len = ipmi_rmcpplus_sendto (c->connection.ipmi_fd,
				      buffer,
				      n,
				      0,
				      (struct sockaddr *)&(c->session.addr),
				      sizeof (struct sockaddr_in));
	} while (len < 0 && errno == EINTR);
    }

  if (len < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_lan_sendto: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      return (-1);
    }

#if 0
  /* don't check, let bad packet timeout */
  if (len != n)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_lan_sendto: invalid bytes written; n=%d; len=%d", n, len));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      return (-1);
    }
#endif

  /* scbuf should be empty now */
  if (!scbuf_is_empty (c->connection.ipmi_to_bmc))
    {
      IPMICONSOLE_CTX_DEBUG (c, ("ipmi_to_bmc not empty"));
      /* Note: Not a fatal error, just return*/
      return (0);
    }

  return (0);
}

/*
 * Return 0 on success
 * Return -1 on fatal error
 */
static int
_asynccomm (ipmiconsole_ctx_t c)
{
  uint8_t tmpbyte;
  ssize_t len;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);

  if ((len = read (c->connection.asynccomm[0], (void *)&tmpbyte, 1)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("read: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      return (-1);
    }

  if (!len)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("asynccomm closed"));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  /* User may have requested several break conditions in a
   * row quickly.  We assume it means just one
   */
  if (tmpbyte == IPMICONSOLE_PIPE_GENERATE_BREAK_CODE)
    {
      if (!(c->session.break_requested))
        {
          int bytes_before_break;

          c->session.break_requested++;

          if ((bytes_before_break = scbuf_used (c->connection.console_remote_console_to_bmc)) < 0)
            {
              IPMICONSOLE_CTX_DEBUG (c, ("scbuf_used: %s", strerror (errno)));
              ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
              return (-1);
            }
          c->session.console_remote_console_to_bmc_bytes_before_break = bytes_before_break;
        }
    }

  return (0);
}

/*
 * Return 0 on success
 * Return -1 on fatal error
 */
static int
_console_read (ipmiconsole_ctx_t c)
{
  char buffer[IPMICONSOLE_PACKET_BUFLEN];
  ssize_t len;
  int n, dropped = 0;
  int secure_malloc_flag;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (!c->session.close_session_flag);

  secure_malloc_flag = (c->config.engine_flags & IPMICONSOLE_ENGINE_LOCK_MEMORY) ? 1 : 0;

  if ((len = read (c->connection.ipmiconsole_fd,
                   buffer,
                   IPMICONSOLE_PACKET_BUFLEN)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("read: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      return (-1);
    }

  if (!len)
    {
      /* Returning -1 closes the session, but really this error is ok
       * since the user is allowed to close the session
       */
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);
      return (-1);
    }

  if ((n = scbuf_write (c->connection.console_remote_console_to_bmc, buffer, len, &dropped, secure_malloc_flag)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("scbuf_write: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (n != len)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("scbuf_write: invalid bytes written; n=%d; len=%d", n, len));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (dropped)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("scbuf_write: dropped data: dropped=%d", dropped));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  return (0);
}

/*
 * Return 0 on success
 * Return -1 on fatal error
 */
static int
_console_write (ipmiconsole_ctx_t c)
{
  char buffer[IPMICONSOLE_PACKET_BUFLEN];
  ssize_t len;
  int n;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (!c->session.close_session_flag);

  /*
   * XXX: Shouldn't assume user will read data fast enough?  I could
   * overrun buffers?
   *
   * Deal with it later.
   */

  if ((n = scbuf_read (c->connection.console_bmc_to_remote_console, buffer, IPMICONSOLE_PACKET_BUFLEN)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("scbuf_read: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((len = write (c->connection.ipmiconsole_fd,
                    buffer,
                    n)) < 0)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("write: %s", strerror (errno)));

      if (errno == EPIPE)
        {
          /* This error is ok since the user is allowed to close the
           * session
           */
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SUCCESS);
        }
      else
        ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      return (-1);
    }

  if (len != n)
    {
      IPMICONSOLE_CTX_DEBUG (c, ("write: invalid bytes written; n=%d; len=%d", n, len));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      return (-1);
    }

  /* scbuf should be empty now */
  if (!scbuf_is_empty (c->connection.console_bmc_to_remote_console))
    {
      IPMICONSOLE_CTX_DEBUG (c, ("console_bmc_to_remote_console not empty"));
      /* Note: Not a fatal error, just return*/
      return (0);
    }

  return (0);
}

static int
_ipmiconsole_poll (struct pollfd *ufds, unsigned int nfds, int timeout)
{
  int n;
  struct timeval tv, tv_orig;
  struct timeval start, end, delta;

  assert (ufds);

  /* prep for EINTR handling */
  if (timeout >= 0)
    {
      /* poll uses timeout in milliseconds */
      tv_orig.tv_sec = (long)timeout/1000;
      tv_orig.tv_usec = (timeout % 1000) * 1000;

      if (gettimeofday(&start, NULL) < 0)
        {
          IPMICONSOLE_DEBUG (("gettimeofday: %s", strerror (errno)));
          return (-1);
        }
    }
  else
    {
      tv_orig.tv_sec = 0;
      tv_orig.tv_usec = 0;
    }

  /* repeat poll if interrupted */
  do
    {
      n = poll(ufds, nfds, timeout);

      if (n < 0 && errno != EINTR)    /* unrecov error */
        {
          IPMICONSOLE_DEBUG (("poll: %s", strerror (errno)));
          return (-1);
        }

      if (n < 0 && timeout >= 0)      /* EINTR - adjust timeout */
        {
          if (gettimeofday(&end, NULL) < 0)
            {
              IPMICONSOLE_DEBUG (("gettimeofday: %s", strerror (errno)));
              return (-1);
            }

          timersub(&end, &start, &delta);     /* delta = end-start */
          timersub(&tv_orig, &delta, &tv);    /* tv = tvsave-delta */
          timeout = (tv.tv_sec * 1000) + (tv.tv_usec/1000);
        }
    } while (n < 0);

  return n;
}

static void *
_ipmiconsole_engine (void *arg)
{
  int perr, ctxs_count = 0;
  unsigned int index;
  unsigned int teardown_flag = 0;
  unsigned int teardown_initiated = 0;

  assert (arg);

  index = *((unsigned int *)arg);

  assert (index < IPMICONSOLE_THREAD_COUNT_MAX);

  free (arg);

  /* No need to exit on failure, probability is low we'll SIGPIPE anyways */
  if (signal (SIGPIPE, SIG_IGN) == SIG_ERR)
    IPMICONSOLE_DEBUG (("signal: %s", strerror (errno)));

  while (!teardown_flag || ctxs_count)
    {
      struct _ipmiconsole_poll_data poll_data;
      int count;
      unsigned int timeout_len;
      unsigned int i;
      int unlock_console_engine_ctxs_mutex_flag = 0;
      int spin_wait_flag = 0;
      char buf[IPMICONSOLE_PIPE_BUFLEN];

      memset (&poll_data, '\0', sizeof (struct _ipmiconsole_poll_data));

      if ((perr = pthread_mutex_lock (&console_engine_teardown_mutex)))
        {
          /* This is one of the only truly "fatal" conditions */
          IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
          teardown_flag = 1;
        }

      if (console_engine_teardown_immediate)
        {
          if ((perr = pthread_mutex_unlock (&console_engine_teardown_mutex)))
            IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
          break;
        }

      if (console_engine_teardown)
        teardown_flag = 1;

      if ((perr = pthread_mutex_unlock (&console_engine_teardown_mutex)))
        {
          /* This is one of the only truly "fatal" conditions */
          IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
          teardown_flag = 1;
        }

      /* Notes:
       *
       * We must lock the list from here till all context data and pointers
       * are retrieved.
       */

      if ((perr = pthread_mutex_lock (&console_engine_ctxs_mutex[index])))
        {
          /* This is one of the only truly "fatal" conditions */
          IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
          teardown_flag = 1;
        }

      /* Note: Set close_session_flag in the contexts before
       * ipmiconsole_process_ctxs(), so the initiation of the closing
       * down will begin now rather than the next iteration of the
       * loop.
       */
      if (teardown_flag && !teardown_initiated)
        {
          /* XXX: Umm, if this fails, we may not be able to teardown
           * cleanly.  Break out of the loop I guess.
           */
          if (list_for_each (console_engine_ctxs[index], _teardown_initiate, NULL) < 0)
            {
              IPMICONSOLE_DEBUG (("list_for_each: %s", strerror (errno)));
              break;
            }
          teardown_initiated++;
        }

      if ((ctxs_count = ipmiconsole_process_ctxs (console_engine_ctxs[index], &timeout_len)) < 0)
        goto continue_loop;

      if (!ctxs_count && teardown_flag)
        continue;

      if (!ctxs_count)
        {
          spin_wait_flag++;
          goto continue_loop;
        }
      poll_data.ctxs_len = ctxs_count;

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

      /*
       * There are 3 pfds per ctx.  One for 'ipmi_fd', 'asynccomm[0]', and 'ipmiconsole_fd'.
       *
       * There is + 1 pfds for the "console_engine_ctxs_notifier".
       * This will be set up manually here, and not in _poll_setup().
       */
      if (!(poll_data.pfds = (struct pollfd *)malloc (((poll_data.ctxs_len * 3) + 1) * sizeof (struct pollfd))))
        {
          IPMICONSOLE_DEBUG (("malloc: %s", strerror (errno)));
          goto continue_loop;
        }

      if (!(poll_data.pfds_ctxs = (ipmiconsole_ctx_t *)malloc (poll_data.ctxs_len * sizeof (ipmiconsole_ctx_t))))
        {
          IPMICONSOLE_DEBUG (("malloc: %s", strerror (errno)));
          goto continue_loop;
        }

      if ((count = list_for_each (console_engine_ctxs[index], _poll_setup, &poll_data)) < 0)
        {
          IPMICONSOLE_DEBUG (("list_for_each: %s", strerror (errno)));
          goto continue_loop;
        }

      if ((perr = pthread_mutex_unlock (&console_engine_ctxs_mutex[index])))
        IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      unlock_console_engine_ctxs_mutex_flag++;

      /* Setup notifier pipe as last remaining poll data */
      poll_data.pfds[(poll_data.ctxs_len * 3)].fd = console_engine_ctxs_notifier[index][0];
      poll_data.pfds[(poll_data.ctxs_len * 3)].events = POLLIN;
      poll_data.pfds[(poll_data.ctxs_len * 3)].revents = 0;

      if (count != ctxs_count)
        {
          IPMICONSOLE_DEBUG (("list_for_each: invalid length returned: %d", count));
          goto continue_loop;
        }

      if (poll_data.pfds_index != ctxs_count)
        {
          IPMICONSOLE_DEBUG (("invalid index set on returned: %d", poll_data.pfds_index));
          goto continue_loop;
        }

      if (_ipmiconsole_poll (poll_data.pfds, (poll_data.ctxs_len * 3) + 1, timeout_len) < 0)
        {
          IPMICONSOLE_DEBUG (("poll: %s", strerror (errno)));
          goto continue_loop;
        }

      for (i = 0; i < poll_data.ctxs_len; i++)
        {
          if (poll_data.pfds[i*3].revents & POLLERR)
            {
              IPMICONSOLE_CTX_DEBUG (poll_data.pfds_ctxs[i], ("POLLERR"));
              /* See comments in _ipmi_recvfrom() regarding ECONNRESET/ECONNREFUSED */
              if (_ipmi_recvfrom (poll_data.pfds_ctxs[i]) < 0)
                {
                  ipmiconsole_ctx_set_errnum (poll_data.pfds_ctxs[i], IPMICONSOLE_ERR_SYSTEM_ERROR);
                  poll_data.pfds_ctxs[i]->session.close_session_flag++;
                  continue;
                }
            }
          if (!poll_data.pfds_ctxs[i]->session.close_session_flag)
            {
              if (poll_data.pfds[i*3+1].revents & POLLNVAL)
                {
                  /* This indicates the user closed the asynccomm file descriptors
                   * which is ok.
                   */
                  IPMICONSOLE_CTX_DEBUG (poll_data.pfds_ctxs[i], ("POLLNVAL"));
                  ipmiconsole_ctx_set_errnum (poll_data.pfds_ctxs[i], IPMICONSOLE_ERR_SUCCESS);
                  poll_data.pfds_ctxs[i]->session.close_session_flag++;
                  continue;
                }
              if (poll_data.pfds[i*3+2].revents & POLLHUP)
                {
                  /* This indicates the user closed the other end of
                   * the socketpair so it's ok.
                   */
                  IPMICONSOLE_CTX_DEBUG (poll_data.pfds_ctxs[i], ("POLLHUP"));
                  ipmiconsole_ctx_set_errnum (poll_data.pfds_ctxs[i], IPMICONSOLE_ERR_SUCCESS);
                  poll_data.pfds_ctxs[i]->session.close_session_flag++;
                  continue;
                }
              if (poll_data.pfds[i*3+1].revents & POLLERR)
                {
                  IPMICONSOLE_CTX_DEBUG (poll_data.pfds_ctxs[i], ("POLLERR"));
                  ipmiconsole_ctx_set_errnum (poll_data.pfds_ctxs[i], IPMICONSOLE_ERR_INTERNAL_ERROR);
                  poll_data.pfds_ctxs[i]->session.close_session_flag++;
                  continue;
                }
              if (poll_data.pfds[i*3+2].revents & POLLERR)
                {
                  IPMICONSOLE_CTX_DEBUG (poll_data.pfds_ctxs[i], ("POLLERR"));
                  ipmiconsole_ctx_set_errnum (poll_data.pfds_ctxs[i], IPMICONSOLE_ERR_INTERNAL_ERROR);
                  poll_data.pfds_ctxs[i]->session.close_session_flag++;
                  continue;
                }
            }
          if (poll_data.pfds[i*3].revents & POLLIN)
            {
              if (_ipmi_recvfrom (poll_data.pfds_ctxs[i]) < 0)
                {
                  poll_data.pfds_ctxs[i]->session.close_session_flag++;
                  continue;
                }
            }
          if (poll_data.pfds[i*3].revents & POLLOUT)
            {
              if (_ipmi_sendto (poll_data.pfds_ctxs[i]) < 0)
                {
                  poll_data.pfds_ctxs[i]->session.close_session_flag++;
                  continue;
                }
            }
          if (poll_data.pfds[i*3 + 1].revents & POLLIN)
            {
              if (_asynccomm (poll_data.pfds_ctxs[i]) < 0)
                {
                  poll_data.pfds_ctxs[i]->session.close_session_flag++;
                  continue;
                }
            }
          if (!poll_data.pfds_ctxs[i]->session.close_session_flag)
            {
              if (poll_data.pfds[i*3+2].revents & POLLIN)
                {
                  if (_console_read (poll_data.pfds_ctxs[i]) < 0)
                    {
                      poll_data.pfds_ctxs[i]->session.close_session_flag++;
                      continue;
                    }
                }
              if (poll_data.pfds[i*3+2].revents & POLLOUT)
                {
                  if (_console_write (poll_data.pfds_ctxs[i]) < 0)
                    {
                      poll_data.pfds_ctxs[i]->session.close_session_flag++;
                      continue;
                    }
                }
            }
        }

      /* We don't care what's read, just get it off the fd */
      if (poll_data.pfds[(poll_data.ctxs_len * 3)].revents & POLLIN)
        {
          if (read (console_engine_ctxs_notifier[index][0], buf, IPMICONSOLE_PIPE_BUFLEN) < 0)
            IPMICONSOLE_DEBUG (("read: %s", strerror (errno)));
        }

    continue_loop:
      if (!unlock_console_engine_ctxs_mutex_flag)
        {
          if ((perr = pthread_mutex_unlock (&console_engine_ctxs_mutex[index])))
            {
              /* This is one of the only truly "fatal" conditions */
              IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
              teardown_flag = 1;
            }
        }
      if (spin_wait_flag)
        {
          /* No contexts stored, either because they all died or none
           * have been submitted yet.  Sleep a little bit to kill some
           * time and avoid spinning.
           */
          /* XXX: Is this portable? */
          usleep (IPMICONSOLE_SPIN_WAIT_TIME);
        }
      free (poll_data.pfds);
      free (poll_data.pfds_ctxs);
    }

  /* No way to return error, so just continue on even if there is a failure */
  if ((perr = pthread_mutex_lock (&console_engine_thread_count_mutex)))
    IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));

  console_engine_thread_count--;

  if ((perr = pthread_mutex_unlock (&console_engine_thread_count_mutex)))
    IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));

  return (NULL);
}

/* Notes: On an error, it is the responsibility of the caller to call
 * ipmiconsole_engine_cleanup() to destroy all previously created
 * threads.
 */
int
ipmiconsole_engine_thread_create (void)
{
  pthread_t thread;
  pthread_attr_t attr;
  unsigned int *index = NULL;
  int perr, rv = -1;

  assert (console_engine_is_setup);

  if ((perr = pthread_mutex_lock (&console_engine_thread_count_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      errno = perr;
      return (-1);
    }

  assert (console_engine_thread_count < IPMICONSOLE_THREAD_COUNT_MAX);

  if ((perr = pthread_attr_init (&attr)))
    {
      IPMICONSOLE_DEBUG (("pthread_attr_init: %s", strerror (perr)));
      errno = perr;
      goto cleanup;
    }

  if ((perr = pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED)))
    {
      IPMICONSOLE_DEBUG (("pthread_attr_setdetachstate: %s", strerror (perr)));
      errno = perr;
      goto cleanup;
    }

  if (!(index = (unsigned int *)malloc (sizeof (unsigned int))))
    {
      IPMICONSOLE_DEBUG (("malloc: %s", strerror (errno)));
      goto cleanup;
    }
  *index = console_engine_thread_count;

  if ((perr = pthread_create (&thread, &attr, _ipmiconsole_engine, index)))
    {
      IPMICONSOLE_DEBUG (("pthread_create: %s", strerror (perr)));
      errno = perr;
      goto cleanup;
    }

  /* Who cares if this fails */
  if ((perr = pthread_attr_destroy (&attr)))
    IPMICONSOLE_DEBUG (("pthread_attr_destroy: %s", strerror (perr)));

  console_engine_thread_count++;

  rv = 0;
 cleanup:

  if ((perr = pthread_mutex_unlock (&console_engine_thread_count_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      errno = perr;
      return (-1);
    }

  return (rv);
}

int
ipmiconsole_engine_submit_ctx (ipmiconsole_ctx_t c)
{
  void *ptr;
  unsigned int i;
  int perr, ret = -1;
  unsigned int min_submitted = UINT_MAX;
  int index = 0;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (!(c->session_submitted));
  assert (console_engine_is_setup);

  if ((perr = pthread_mutex_lock (&console_engine_thread_count_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      return (-1);
    }

  for (i = 0; i < console_engine_thread_count; i++)
    {
      if ((perr = pthread_mutex_lock (&console_engine_ctxs_mutex[i])))
        {
          IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          goto cleanup_thread_count;
        }

      if (console_engine_ctxs_count[i] < min_submitted)
        {
          min_submitted = console_engine_ctxs_count[i];
          index = i;
        }

      if ((perr = pthread_mutex_unlock (&console_engine_ctxs_mutex[i])))
        {
          IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
          ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
          goto cleanup_thread_count;
        }
    }

  if ((perr = pthread_mutex_lock (&console_engine_ctxs_mutex[index])))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      goto cleanup_thread_count;
    }

  if (!(ptr = list_append (console_engine_ctxs[index], c)))
    {
      /* Note: Don't do a CTX debug, this is more of a global debug */
      IPMICONSOLE_DEBUG (("list_append: %s", strerror (errno)));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      goto cleanup_ctxs;
    }

  if (ptr != (void *)c)
    {
      IPMICONSOLE_DEBUG (("list_append: invalid pointer: ptr=%p; c=%p", ptr, c));
      ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_INTERNAL_ERROR);
      goto cleanup_ctxs;
    }

  console_engine_ctxs_count[index]++;

  ret = 0;

  /* achu:
   *
   * Necessary to set this here b/c at this point in time, the context
   * is submitted, so the engine will be doing its own cleanup
   * (garbage collector, etc.).
   */
  c->session_submitted++;

  /* "Interrupt" the engine and tell it to get moving along w/ the new context */
  if (write (console_engine_ctxs_notifier[index][1], "1", 1) < 0)
    IPMICONSOLE_DEBUG (("write: %s", strerror (errno)));

 cleanup_ctxs:
  if ((perr = pthread_mutex_unlock (&console_engine_ctxs_mutex[index])))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      goto cleanup_thread_count;
    }

 cleanup_thread_count:
  if ((perr = pthread_mutex_unlock (&console_engine_thread_count_mutex)))
    IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));

  return (ret);
}

int
ipmiconsole_engine_cleanup (int cleanup_sol_sessions)
{
  unsigned int i;
  unsigned int thread_count;
  int perr, rv = -1;

  if ((perr = pthread_mutex_lock (&console_engine_is_setup_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      return (-1);
    }

  if (!console_engine_is_setup)
    goto unlock_is_setup_mutex;

  if ((perr = pthread_mutex_lock (&console_engine_thread_count_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      goto unlock_is_setup_mutex;
    }

  thread_count = console_engine_thread_count;

  if ((perr = pthread_mutex_unlock (&console_engine_thread_count_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      goto unlock_is_setup_mutex;
    }

  if (!thread_count)
    {
      rv = 0;
      goto engine_cleanup;
    }

  if ((perr = pthread_mutex_lock (&console_engine_teardown_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      goto engine_cleanup;
    }

  console_engine_teardown++;
  if (!cleanup_sol_sessions)
    console_engine_teardown_immediate++;

  if ((perr = pthread_mutex_unlock (&console_engine_teardown_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      goto engine_cleanup;
    }

  /* "Interrupt" the engine thread and tell it to get moving along */
  for (i = 0; i < console_engine_ctxs_notifier_num; i++)
    {
      if (write (console_engine_ctxs_notifier[i][1], "1", 1) < 0)
        IPMICONSOLE_DEBUG (("write: %s", strerror (errno)));
    }

  if ((perr = pthread_mutex_lock (&console_engine_thread_count_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      goto engine_cleanup;
    }

  while (console_engine_thread_count)
    {
      if ((perr = pthread_mutex_unlock (&console_engine_thread_count_mutex)))
        {
          IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
          goto engine_cleanup;
        }

      /* Wait awhile then try again */
      /* XXX: Is this portable? */
      usleep (IPMICONSOLE_SPIN_WAIT_TIME);

      if ((perr = pthread_mutex_lock (&console_engine_thread_count_mutex)))
        {
          IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
          goto engine_cleanup;
        }
    }

  if ((perr = pthread_mutex_unlock (&console_engine_thread_count_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      goto engine_cleanup;
    }

  /* "Interrupt" the garbage collector and tell it to quit */
  if (write (garbage_collector_notifier[1], "1", 1) < 0)
    IPMICONSOLE_DEBUG (("write: %s", strerror (errno)));

  if ((perr = pthread_mutex_lock (&garbage_collector_active_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      goto engine_cleanup;
    }

  while (garbage_collector_active)
    {
      if ((perr = pthread_cond_wait (&garbage_collector_active_cond,
				     &garbage_collector_active_mutex)))
	{
	  IPMICONSOLE_DEBUG (("pthread_cond_wait: %s", strerror (perr)));
	  goto engine_cleanup;
	}
    }

  if ((perr = pthread_mutex_unlock (&garbage_collector_active_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      goto engine_cleanup;
    }

 engine_cleanup:
  for (i = 0; i < IPMICONSOLE_THREAD_COUNT_MAX; i++)
    {
      if (console_engine_ctxs[i])
        list_destroy (console_engine_ctxs[i]);
      console_engine_ctxs[i] = NULL;
      console_engine_ctxs[i] = 0;
      pthread_mutex_destroy (&console_engine_ctxs_mutex[i]);
      /* ignore potential error, cleanup path */
      close (console_engine_ctxs_notifier[i][0]);
      /* ignore potential error, cleanup path */
      close (console_engine_ctxs_notifier[i][1]);
    }
  /* ignore potential error, cleanup path */
  close (garbage_collector_notifier[0]);
  /* ignore potential error, cleanup path */
  close (garbage_collector_notifier[1]);

  /* achu: The engine threads have been torn down, all the contexts
   * managed by those threads have been moved to
   * console_engine_ctxs_to_destroy, and the garbage collector has
   * been shut down.  So we don't need to lock w/ the
   * console_engine_ctxs_to_destroy_mutex.
   * 
   * This list destruction could race w/ API code b/c the user could
   * still be running around trying to use the contexts post
   * engine_teardown() (e.g. calling ipmiconsole_ctx_destroy() after
   * calling ipmiconsole_engine_teardown()).  We assume the user won't
   * do this.
   */

  list_destroy (console_engine_ctxs_to_destroy);
  console_engine_ctxs_to_destroy = NULL;

  /* ignore potential error, cleanup path */
  close (dummy_fd);
  dummy_fd = -1;

  console_engine_is_setup = 0;

  if ((perr = pthread_mutex_lock (&console_engine_teardown_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
      goto unlock_is_setup_mutex;
    }

  console_engine_teardown = 0;
  console_engine_teardown_immediate = 0;

  if ((perr = pthread_mutex_unlock (&console_engine_teardown_mutex)))
    {
      IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
      goto unlock_is_setup_mutex;
    }

  rv = 0;
 unlock_is_setup_mutex:
  if ((perr = pthread_mutex_unlock (&console_engine_is_setup_mutex)))
    IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));

  return (rv);
}
