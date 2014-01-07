/*****************************************************************************\
 *  $Id: ipmipower_error.c,v 1.4 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
/*****************************************************************************\
 *  $Id: ipmipower_error.c,v 1.4 2010-02-08 22:02:31 chu11 Exp $
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

#include "ipmipower.h"

#include "ipmipower_error.h"

#include "freeipmi-portability.h"
#include "fd.h"

static unsigned int power_error_flags = 0;

int
ipmipower_error_setup (unsigned int error_flags)
{
  assert (!(error_flags & ~(IPMIPOWER_ERROR_STDERR | IPMIPOWER_ERROR_SYSLOG)));

  power_error_flags = error_flags;

  return (0);
}

static void
_error (const char *fmt, va_list ap)
{
  char errbuf[IPMIPOWER_ERROR_BUFLEN];
  int len;

  assert (fmt);

  len = vsnprintf (errbuf, IPMIPOWER_ERROR_BUFLEN, fmt, ap);
  if (power_error_flags & IPMIPOWER_ERROR_STDERR)
    {
      fprintf (stderr, "%s\r\n", errbuf);
      fflush (stderr);
    }
  if (power_error_flags & IPMIPOWER_ERROR_SYSLOG)
    syslog (LOG_ERR, "%s", errbuf);
}

void
ipmipower_error (const char *fmt, ...)
{
  va_list ap;

  assert (fmt);

  va_start (ap, fmt);
  _error (fmt, ap);
  va_end (ap);
}

static void
_debug (const char *fmt, va_list ap)
{
  char errbuf[IPMIPOWER_ERROR_BUFLEN];
  int len;

  assert (fmt);

  len = vsnprintf (errbuf, IPMIPOWER_ERROR_BUFLEN, fmt, ap);
  if (power_error_flags & IPMIPOWER_ERROR_STDERR)
    {
      fprintf (stderr, "%s\r\n", errbuf);
      fflush (stderr);
    }
  if (power_error_flags & IPMIPOWER_ERROR_SYSLOG)
    syslog (LOG_DEBUG, "%s", errbuf);
}

void
ipmipower_debug (const char *fmt, ...)
{
  va_list ap;

  assert (fmt);

  va_start (ap, fmt);
  _debug (fmt, ap);
  va_end (ap);
}

char *
__error_msg_create (const char *fmt, ...)
{
  char *buffer;
  va_list ap;

  assert (fmt);

  if (!(buffer = malloc (IPMIPOWER_ERROR_BUFLEN)))
    return (NULL);

  va_start (ap, fmt);
  vsnprintf (buffer, IPMIPOWER_ERROR_BUFLEN, fmt, ap);
  va_end (ap);

  return (buffer);
}
