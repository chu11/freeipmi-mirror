/*****************************************************************************\
 *  $Id: ipmidetectd.c,v 1.17 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2012 Lawrence Livermore National Security, LLC.
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

#include "freeipmi-portability.h"
#include "daemon-util.h"
#include "error.h"

#define IPMIDETECTD_PIDFILE IPMIDETECTD_LOCALSTATEDIR "/run/ipmidetectd.pid"

extern struct ipmidetectd_config conf;

int
main (int argc, char **argv)
{
  err_init (argv[0]);
  err_set_flags (ERROR_STDOUT);

  ipmidetectd_config_setup (argc, argv);

  if (!conf.debug)
    {
      daemonize_common (IPMIDETECTD_PIDFILE);
      err_set_flags (ERROR_SYSLOG);
    }
  else
    err_set_flags (ERROR_STDERR);

  daemon_signal_handler_setup (NULL);

  /* Call after daemonization, since daemonization closes currently
   * open fds
   */
  openlog (argv[0], LOG_ODELAY | LOG_PID, LOG_DAEMON);

  ipmidetectd_loop ();

  return (0);
}
