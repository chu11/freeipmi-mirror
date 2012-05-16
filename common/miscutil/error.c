/*****************************************************************************\
 *  $Id: error.c,v 1.2 2008-08-12 18:14:33 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2005 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>.
 *  UCRL-CODE-155989 All rights reserved.
 *
 *  This file is part of Cerebro, a collection of cluster monitoring
 *  tools and libraries.  For details, see
 *  <http://www.llnl.gov/linux/cerebro/>.
 *
 *  Cerebro is free software; you can redistribute it and/or modify it under
 *  the terms of the GNU General Public License as published by the Free
 *  Software Foundation; either version 2 of the License, or (at your option)
 *  any later version.
 *
 *  Cerebro is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 *  details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Genders; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <syslog.h>
#include <assert.h>
#include <errno.h>

#include "error.h"

static char *err_prog = NULL;  
static int err_flags = 0;
static int exit_value = EXIT_FAILURE;

#define ERROR_BUFLEN   1024

void 
err_init(char *prog)
{
  char *p = strrchr(prog, '/');
  err_prog = p ? p + 1 : prog;
}

void
err_init_exit_value(int val)
{
  exit_value = val;
}

int 
err_get_flags(void)
{
  assert(err_prog);

  return err_flags;
}

void 
err_set_flags(int flags)
{
  assert(err_prog);

  err_flags = flags;
}

static void 
_err(int syslog_level, const char *fmt, va_list ap)
{
  char buf[ERROR_BUFLEN];

  assert(err_prog);

  vsnprintf(buf, ERROR_BUFLEN, fmt, ap);
  if (err_flags & ERROR_STDOUT)
    fprintf(stdout, "%s: %s\n", err_prog, buf);
  if (err_flags & ERROR_STDERR)
    fprintf(stderr, "%s: %s\n", err_prog, buf);
  if (err_flags & ERROR_SYSLOG)
    syslog(syslog_level, "%s", buf);
}

void
err_debug(const char *fmt, ...)
{
  va_list ap;

  assert(err_prog);

  va_start(ap, fmt);
  _err(LOG_DEBUG, fmt, ap);
  va_end(ap);
}

void 
err_output(const char *fmt, ...)
{
  va_list ap;

  assert(err_prog);

  va_start(ap, fmt);
  _err(LOG_ERR, fmt, ap);
  va_end(ap);
}

void
err_exit(const char *fmt, ...)
{
  va_list ap;

  assert(err_prog);

  va_start(ap, fmt);
  _err(LOG_ERR, fmt, ap);
  va_end(ap);
  exit(exit_value);
}
