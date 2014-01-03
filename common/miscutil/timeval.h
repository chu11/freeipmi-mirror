/*****************************************************************************\
 *  $Id: timeval.h,v 1.6 2010-02-08 22:02:30 chu11 Exp $
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

#ifndef TIMEVAL_H
#define TIMEVAL_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

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

void timeval_clear (struct timeval *a);

int timeval_gt (struct timeval *a, struct timeval *b);

int timeval_lt (struct timeval *a, struct timeval *b);

void timeval_add (struct timeval *a, struct timeval *b, struct timeval *result);

void timeval_sub (struct timeval *a, struct timeval *b, struct timeval *result);

void timeval_millisecond_init (struct timeval *a, unsigned int ms);

void timeval_add_ms (struct timeval *a, unsigned int ms, struct timeval *result);

void timeval_sub_ms (struct timeval *a, unsigned int ms, struct timeval *result);

void timeval_millisecond_calc (struct timeval *a, unsigned int *ms);

#endif /* TIMEVAL_H */
