/*****************************************************************************\
 *  $Id: ipmidetectd.c,v 1.17 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2011 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <syslog.h>
#include <sys/types.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/stat.h>
#include <signal.h>
#include <assert.h>
#include <errno.h>

#include "ipmidetectd_config.h"
#include "ipmidetectd_loop.h"
#include "debug.h"

#include "freeipmi-portability.h"
#include "error.h"

extern struct ipmidetectd_config conf;

static void
_daemon_init (void)
{
  /* Based on code in Unix network programming by R. Stevens */
  pid_t pid;
  unsigned int i;

  if ((pid = fork ()) < 0)
    IPMIDETECTD_EXIT (("fork: %s", strerror (errno)));

  if (pid != 0)                 /* Terminate Parent */
    exit (0);

  setsid ();

  if (signal (SIGHUP, SIG_IGN) == SIG_ERR)
    IPMIDETECTD_EXIT (("signal: %s", strerror (errno)));

  if ((pid = fork ()) < 0)
    IPMIDETECTD_EXIT (("fork: %s", strerror (errno)));

  if (pid != 0)                 /* Terminate 1st Child */
    exit (0);

  chdir ("/");

  umask (0);

  for (i = 0; i < 64; i++)
    close (i);
}

int
main (int argc, char **argv)
{
  err_init (argv[0]);
  err_set_flags (ERROR_STDOUT);

  ipmidetectd_config_setup (argc, argv);

#ifndef NDEBUG
  if (!conf.debug)
    {
      _daemon_init ();
      err_set_flags (ERROR_SYSLOG);
    }
  else
    err_set_flags (ERROR_STDERR);
#else  /* NDEBUG */
  _daemon_init ();
  err_set_flags (ERROR_SYSLOG);
#endif /* NDEBUG */

  /* Call after daemonization, since daemonization closes currently
   * open fds
   */
  openlog (argv[0], LOG_ODELAY | LOG_PID, LOG_DAEMON);

  ipmidetectd_loop ();

  return (0);
}
