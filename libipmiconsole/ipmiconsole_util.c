/*****************************************************************************\
 *  $Id: ipmiconsole_util.c,v 1.13 2010-02-08 22:02:30 chu11 Exp $
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <assert.h>
#include <errno.h>

#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

#include "ipmiconsole_util.h"
#include "ipmiconsole_ctx.h"
#include "ipmiconsole_debug.h"
#include "ipmiconsole_engine.h"

#include "freeipmi-portability.h"
#include "list.h"

int
ipmiconsole_set_closeonexec (ipmiconsole_ctx_t c, int fd)
{
  int closeonexec;

  /* User need not pass in valid context for this function */

  if ((closeonexec = fcntl (fd, F_GETFD, 0)) < 0)
    {
      IPMICONSOLE_DEBUG (("fcntl: %s", strerror (errno)));
      if (c && c->magic == IPMICONSOLE_CTX_MAGIC)
        ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      return (-1);
    }
  closeonexec |= FD_CLOEXEC;
  if (fcntl (fd, F_SETFD, closeonexec) < 0)
    {
      IPMICONSOLE_DEBUG (("fcntl: %s", strerror (errno)));
      if (c && c->magic == IPMICONSOLE_CTX_MAGIC)
        ipmiconsole_ctx_set_errnum (c, IPMICONSOLE_ERR_SYSTEM_ERROR);
      return (-1);
    }

  return (0);
}
