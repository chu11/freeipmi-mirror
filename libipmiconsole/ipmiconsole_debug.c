/*****************************************************************************\
 *  $Id: ipmiconsole_debug.c,v 1.21 2010-02-08 22:02:30 chu11 Exp $
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
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#if HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <syslog.h>
#include <assert.h>
#include <errno.h>

#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

#include "ipmiconsole_debug.h"

#include "freeipmi-portability.h"
#include "fd.h"

static uint32_t console_debug_flags = 0;
static int console_debug_fd = -1;
static pthread_mutex_t console_stdout_debug_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t console_stderr_debug_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t console_file_debug_mutex = PTHREAD_MUTEX_INITIALIZER;

int
ipmiconsole_debug_setup (uint32_t debug_flags)
{
  assert (debug_flags == IPMICONSOLE_DEBUG_DEFAULT
	  || !(debug_flags & ~IPMICONSOLE_DEBUG_MASK));

  if (debug_flags == IPMICONSOLE_DEBUG_DEFAULT)
    console_debug_flags = 0;
  else
    console_debug_flags = debug_flags;

  if (console_debug_flags & IPMICONSOLE_DEBUG_FILE)
    {
      char filename[MAXPATHLEN];
      pid_t pid;

      pid = getpid();

      snprintf (filename,
                MAXPATHLEN,
                "%s.%d",
                IPMICONSOLE_DEBUG_FILENAME,
		pid);

      if ((console_debug_fd = open (filename,
				    O_CREAT | O_APPEND | O_WRONLY | O_EXCL,
				    0600)) < 0)
        {
          console_debug_flags &= ~IPMICONSOLE_DEBUG_FILE;
          IPMICONSOLE_DEBUG (("open: %s", strerror (errno)));
          console_debug_flags = 0;
          return (-1);
        }
    }

  return (0);
}

void
ipmiconsole_debug_cleanup (void)
{
  if (console_debug_flags & IPMICONSOLE_DEBUG_FILE && console_debug_fd >= 0)
    {
      /* ignore potential error, cleanup path */
      close (console_debug_fd);
      console_debug_fd = -1;
    }
  console_debug_flags = 0;
}

static void
_debug (const char *fmt, va_list ap)
{
  char errbuf[IPMICONSOLE_DEBUG_ERROR_BUFLEN];
  int len, perr;

  assert (fmt);

  len = vsnprintf (errbuf, IPMICONSOLE_DEBUG_ERROR_BUFLEN, fmt, ap);
  if (console_debug_flags & IPMICONSOLE_DEBUG_STDOUT)
    {
      if ((perr = pthread_mutex_lock (&console_stdout_debug_mutex)))
        {
          console_debug_flags &= ~IPMICONSOLE_DEBUG_STDOUT;
          IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
          goto try_stderr;
        }

      fprintf (stdout, "%s\r\n", errbuf);
      fflush (stdout);

      if ((perr = pthread_mutex_unlock (&console_stdout_debug_mutex)))
        {
          console_debug_flags &= ~IPMICONSOLE_DEBUG_STDOUT;
          IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
          goto try_stderr;
        }
    }
 try_stderr:
  if (console_debug_flags & IPMICONSOLE_DEBUG_STDERR)
    {
      if ((perr = pthread_mutex_lock (&console_stderr_debug_mutex)))
        {
          console_debug_flags &= ~IPMICONSOLE_DEBUG_STDERR;
          IPMICONSOLE_DEBUG (("pthread_mutex_lock: %s", strerror (perr)));
          goto try_syslog;
        }

      fprintf (stderr, "%s\r\n", errbuf);
      fflush (stderr);

      if ((perr = pthread_mutex_unlock (&console_stderr_debug_mutex)))
        {
          console_debug_flags &= ~IPMICONSOLE_DEBUG_STDERR;
          IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
          goto try_syslog;
        }
    }
 try_syslog:
  if (console_debug_flags & IPMICONSOLE_DEBUG_SYSLOG)
    syslog (LOG_DEBUG, "%s", errbuf);
  if (console_debug_flags & IPMICONSOLE_DEBUG_FILE)
    {
      char tbuf[IPMICONSOLE_DEBUG_ERROR_BUFLEN+2];
      int tlen;

      tlen = snprintf (tbuf, IPMICONSOLE_DEBUG_ERROR_BUFLEN+2, "%s\n", errbuf);

      if ((perr = pthread_mutex_lock (&console_file_debug_mutex)))
        {
          console_debug_flags &= ~IPMICONSOLE_DEBUG_FILE;
          IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
          goto out;
        }

      if ((fd_write_n (console_debug_fd, tbuf, tlen)) < 0)
        {
          console_debug_flags &= ~IPMICONSOLE_DEBUG_FILE;
          IPMICONSOLE_DEBUG (("fd_write_n: %s", strerror (errno)));
          /* fall-through to try and unlock */
        }

      if ((perr = pthread_mutex_unlock (&console_file_debug_mutex)))
        {
          console_debug_flags &= ~IPMICONSOLE_DEBUG_FILE;
          IPMICONSOLE_DEBUG (("pthread_mutex_unlock: %s", strerror (perr)));
          goto out;
        }
    }
 out:
  /* Shutup gcc */
  ;
}

void
ipmiconsole_debug (const char *fmt, ...)
{
  va_list ap;

  assert (fmt);

  va_start (ap, fmt);
  _debug (fmt, ap);
  va_end (ap);
}

static void
_ctx_debug (ipmiconsole_ctx_t c, const char *fmt, va_list ap)
{
  char errbuf[IPMICONSOLE_DEBUG_ERROR_BUFLEN];
  int len, perr;

  assert (fmt);

  len = vsnprintf (errbuf, IPMICONSOLE_DEBUG_ERROR_BUFLEN, fmt, ap);
  if (c->config.debug_flags & IPMICONSOLE_DEBUG_STDOUT)
    {
      if ((perr = pthread_mutex_lock (&console_stdout_debug_mutex)))
        {
          c->config.debug_flags &= ~IPMICONSOLE_DEBUG_STDOUT;
          IPMICONSOLE_CTX_DEBUG (c, ("pthread_mutex_lock: %s", strerror (perr)));
          goto try_stderr;
        }

      fprintf (stdout, "%s\r\n", errbuf);
      fflush (stdout);

      if ((perr = pthread_mutex_unlock (&console_stdout_debug_mutex)))
        {
          c->config.debug_flags &= ~IPMICONSOLE_DEBUG_STDOUT;
          IPMICONSOLE_CTX_DEBUG (c, ("pthread_mutex_unlock: %s", strerror (perr)));
          goto try_stderr;
        }
    }
 try_stderr:
  if (c->config.debug_flags & IPMICONSOLE_DEBUG_STDERR)
    {
      if ((perr = pthread_mutex_lock (&console_stderr_debug_mutex)))
        {
          c->config.debug_flags &= ~IPMICONSOLE_DEBUG_STDERR;
          IPMICONSOLE_CTX_DEBUG (c, ("pthread_mutex_lock: %s", strerror (perr)));
          goto try_syslog;
        }

      fprintf (stderr, "%s\r\n", errbuf);
      fflush (stderr);

      if ((perr = pthread_mutex_unlock (&console_stderr_debug_mutex)))
        {
          c->config.debug_flags &= ~IPMICONSOLE_DEBUG_STDERR;
          IPMICONSOLE_CTX_DEBUG (c, ("pthread_mutex_unlock: %s", strerror (perr)));
          goto try_syslog;
        }
    }
 try_syslog:
  if (c->config.debug_flags & IPMICONSOLE_DEBUG_SYSLOG)
    syslog (LOG_DEBUG, "%s", errbuf);
  if (c->config.debug_flags & IPMICONSOLE_DEBUG_FILE)
    {
      char tbuf[IPMICONSOLE_DEBUG_ERROR_BUFLEN+2];
      int tlen;

      tlen = snprintf (tbuf, IPMICONSOLE_DEBUG_ERROR_BUFLEN+2, "%s\n", errbuf);

      /* Note: This is a per-ctx file descriptor, so thread syncing
       * isn't required
       */
      if ((fd_write_n (c->debug.debug_fd, tbuf, tlen)) < 0)
        {
          c->config.debug_flags &= ~IPMICONSOLE_DEBUG_FILE;
          IPMICONSOLE_CTX_DEBUG (c, ("fd_write_n: %s", strerror (errno)));
        }
    }
}

void
ipmiconsole_ctx_debug (ipmiconsole_ctx_t c, const char *fmt, ...)
{
  va_list ap;

  assert (c);
  assert (c->magic == IPMICONSOLE_CTX_MAGIC);
  assert (fmt);

  va_start (ap, fmt);
  _ctx_debug (c, fmt, ap);
  va_end (ap);
}

char *
__debug_msg_create (const char *fmt, ...)
{
  char *buffer;
  va_list ap;

  assert (fmt);

  if (!(buffer = malloc (IPMICONSOLE_DEBUG_ERROR_BUFLEN)))
    return (NULL);

  va_start (ap, fmt);
  vsnprintf (buffer, IPMICONSOLE_DEBUG_ERROR_BUFLEN, fmt, ap);
  va_end (ap);

  return (buffer);
}
