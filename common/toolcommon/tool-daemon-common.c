/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
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
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/select.h>
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
#include <signal.h>
#include <errno.h>
#include <assert.h>

#include "tool-daemon-common.h"

#include "freeipmi-portability.h"
#include "error.h"

static char *daemon_pidfile = NULL;

static sighandler_t daemon_cb = NULL;

int
daemonize_common (const char *pidfile)
{
  unsigned int i;
  pid_t pid;
  int fds[2];
  
  assert (pidfile);

  /* Based on code in Unix network programming by R. Stevens */

  if (pipe (fds) < 0 )
    err_exit ("pipe: %s", strerror (errno));

  if ((pid = fork ()) < 0)
    err_exit ("fork: %s", strerror (errno));
  if (pid)
    {
      /* parent terminates */
      char buf;
      if (read(fds[0], &buf, 1) < 0)
	err_exit ("read: %s", strerror (errno));
      close(fds[1]);
      close(fds[0]);
      exit (0);
    }

  setsid ();

  if (signal (SIGHUP, SIG_IGN) == SIG_ERR)
    err_exit ("signal: %s", strerror (errno));

  if ((pid = fork ()) < 0)
    err_exit ("fork: %s", strerror (errno));

  if (pid)
    {
      FILE *pf;
          
      /* Do not want pidfile writable to group/other */
      umask(022);
          
      (void) unlink (pidfile);

      if (!(pf = fopen (pidfile, "w")))
	err_exit ("fopen: %s", strerror (errno));

      /* write the 2nd child PID to the pidfile */
      fprintf (pf, "%u\n", pid);
      fclose (pf);

      exit (0);                   /* 1st child terminates */
    }
      
  if (chdir ("/") < 0)
    err_exit ("chdir: %s", strerror (errno));

  umask (0);

  if (write(fds[1], "a", 1) < 0)
    err_exit ("write: %s", strerror (errno));
  close(fds[1]);
  close(fds[0]);
  for (i = 0; i < 64; i++)
    close (i);

  daemon_pidfile = (char *)pidfile;

  return (0);
}

static void
_daemon_shutdown_signal_handler (int sig)
{
  /* user may not have called daemonize(), perhaps running in debug mode */
  if (daemon_pidfile)
    (void) unlink (daemon_pidfile);

  if (daemon_cb)
    daemon_cb (sig);
}

int
daemon_signal_handler_setup (sighandler_t cb)
{
  /* cb can NULL, for no callback */

  if (signal (SIGTERM, _daemon_shutdown_signal_handler) == SIG_ERR)
    err_exit ("signal: %s", strerror (errno));

  if (signal (SIGINT, _daemon_shutdown_signal_handler) == SIG_ERR)
    err_exit ("signal: %s", strerror (errno));

  if (signal (SIGQUIT, _daemon_shutdown_signal_handler) == SIG_ERR)
    err_exit ("signal: %s", strerror (errno));

  daemon_cb = cb;
  return (0);
}

int
daemon_sleep (unsigned int sleep_len)
{
  struct timeval tv;
  
  if (!sleep_len)
    return (0);
  
  tv.tv_sec = sleep_len;
  tv.tv_usec = 0;
  if (select (1, NULL, NULL, NULL, &tv) < 0)
    {
      if (errno != EINTR)
        err_exit ("select: %s", strerror (errno));
    }
  return (0);
}
