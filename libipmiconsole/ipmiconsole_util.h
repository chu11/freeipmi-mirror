/*****************************************************************************\
 *  $Id: ipmiconsole_util.h,v 1.10 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2015 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see https://savannah.gnu.org/projects/freeipmi/.
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

#ifndef IPMICONSOLE_UTIL_H
#define IPMICONSOLE_UTIL_H

#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */

#include "ipmiconsole.h"

int ipmiconsole_set_closeonexec (ipmiconsole_ctx_t c, int fd);

/* ipmiconsole_gettime
 * - fill tv from clock_gettime(CLOCK_MONOTONIC) when available, else
 *   gettimeofday()
 * - for elapsed time / timeout arithmetic only, not calendar time
 * - returns 0 on success, -1 on error with errno set
 */
int ipmiconsole_gettime (struct timeval *tv);

#endif /* IPMICONSOLE_UTIL_H */
