/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif  /* !TIME_WITH_SYS_TIME */
#include <assert.h>
#include <errno.h>

#include "libcommon/ipmi-time-util.h"

#include "freeipmi-portability.h"

int
gettime_monotonic (struct timeval *tv)
{
  assert (tv);

#if defined(HAVE_CLOCK_GETTIME) && defined(HAVE_CLOCK_MONOTONIC)
  {
    struct timespec ts;

    if (clock_gettime (CLOCK_MONOTONIC, &ts) < 0)
      return (-1);

    tv->tv_sec = ts.tv_sec;
    tv->tv_usec = ts.tv_nsec / 1000;
  }
#else
  if (gettimeofday (tv, NULL) < 0)
    return (-1);
#endif

  return (0);
}
