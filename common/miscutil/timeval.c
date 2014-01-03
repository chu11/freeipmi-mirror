/*****************************************************************************\
 *  $Id: timeval.c,v 1.6 2010-02-08 22:02:30 chu11 Exp $
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
#include <assert.h>
#include <errno.h>

#include "timeval.h"

#define TIMEVAL_MICROSECONDS_IN_SECOND      1000000
#define TIMEVAL_MILLISECONDS_IN_SECOND      1000
#define TIMEVAL_MICROSECONDS_IN_MILLISECOND 1000

/*
 * Note:
 *
 * These are primarily portable versions of
 *
 * timerisset
 * timerclear
 * timercmp
 * timeradd
 * timersub
 *
 * and some additional wrapper functions for milliseconds
 */

void
timeval_clear (struct timeval *a)
{
  assert (a);

  a->tv_sec = a->tv_usec = 0;
}

int
timeval_gt (struct timeval *a, struct timeval *b)
{
  assert (a);
  assert (b);

  if (a->tv_sec == b->tv_sec)
    return (a->tv_usec > b->tv_usec);
  return (a->tv_sec > b->tv_sec);
}

int
timeval_lt (struct timeval *a, struct timeval *b)
{
  assert (a);
  assert (b);

  if (a->tv_sec == b->tv_sec)
    return (a->tv_usec < b->tv_usec);
  return (a->tv_sec < b->tv_sec);
}

void
timeval_add (struct timeval *a, struct timeval *b, struct timeval *result)
{
  assert (a);
  assert (b);
  assert (result);

  result->tv_sec = a->tv_sec + b->tv_sec;
  result->tv_usec = a->tv_usec + b->tv_usec;
  if (result->tv_usec >= TIMEVAL_MICROSECONDS_IN_SECOND)
    {
      result->tv_sec++;
      result->tv_usec -= TIMEVAL_MICROSECONDS_IN_SECOND;
    }
}

void
timeval_sub (struct timeval *a, struct timeval *b, struct timeval *result)
{
  assert (a);
  assert (b);
  assert (result);


  if (timeval_gt (a, b))
    {
      result->tv_sec = a->tv_sec - b->tv_sec;
      result->tv_usec = a->tv_usec - b->tv_usec;
      if (result->tv_usec < 0)
        {
          result->tv_sec--;
          result->tv_usec += TIMEVAL_MICROSECONDS_IN_SECOND;
        }
    }
  else
    {
      result->tv_sec = 0;
      result->tv_usec = 0;
    }
}

void
timeval_millisecond_init (struct timeval *a, unsigned int ms)
{
  assert (a);

  timeval_clear (a);
  a->tv_sec = ms/TIMEVAL_MILLISECONDS_IN_SECOND;
  ms -= (a->tv_sec * TIMEVAL_MILLISECONDS_IN_SECOND);
  a->tv_usec = ms * TIMEVAL_MICROSECONDS_IN_MILLISECOND;
}

void
timeval_add_ms (struct timeval *a, unsigned int ms, struct timeval *result)
{
  struct timeval b;

  assert (a);
  assert (result);

  timeval_millisecond_init (&b, ms);
  timeval_add (a, &b, result);
}

void
timeval_sub_ms (struct timeval *a, unsigned int ms, struct timeval *result)
{
  struct timeval b;

  assert (a);
  assert (result);

  timeval_millisecond_init (&b, ms);
  timeval_sub (a, &b, result);
}

void
timeval_millisecond_calc (struct timeval *a, unsigned int *ms)
{
  unsigned int t;

  assert (a);
  assert (ms);

  t = 0;
  t += a->tv_sec * TIMEVAL_MILLISECONDS_IN_SECOND;
  t += a->tv_usec / TIMEVAL_MICROSECONDS_IN_MILLISECOND;
  /* Note: Must round up.  Otherwise time will always "disappear". */
  if (a->tv_usec % TIMEVAL_MICROSECONDS_IN_MILLISECOND)
    t += 1;
  *ms = t;
}
