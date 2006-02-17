/*****************************************************************************\
 *  $Id: ipmipower_util.c,v 1.1.4.6 2006-02-17 23:59:49 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *  
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmipower is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmipower is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#include <errno.h>
#include <assert.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/mman.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include "ipmipower_util.h"
#include "ipmipower_wrappers.h"

#define MICROSECS_IN_SECOND     1000000
#define MICROSECS_IN_MILLISEC   1000

#define DEVURANDOM              "/dev/urandom"
#define DEVRANDOM               "/dev/random"

uint32_t
get_rand(void)
{
  u_int32_t val;

  if (ipmi_get_random((char *)&val, sizeof(val)) < 0)
    {
      dbg("get_rand: ipmi_get_random: %s", strerror(errno));
      return (u_int32_t)rand();
    }

  return val;
}

void
millisec_add(struct timeval *old, struct timeval *new, unsigned int ms)
{
  unsigned int micro;
  
  assert(old != NULL && new != NULL);
  assert(old != new);

  micro = old->tv_usec + (ms * MICROSECS_IN_MILLISEC);
  
  if (micro > MICROSECS_IN_SECOND) 
    {
      new->tv_sec = old->tv_sec + (micro/MICROSECS_IN_SECOND);
      new->tv_usec = (micro % MICROSECS_IN_SECOND);
    }
  else
    {
      new->tv_sec = old->tv_sec;
      new->tv_usec = micro;
    }
}

int
millisec_diff(struct timeval *after, struct timeval *before)
{
  unsigned int micro = 0;

  assert(before != NULL && after != NULL);
  assert(before != after);

  if (after->tv_sec < before->tv_sec)
    return 0;
  else if (before->tv_sec == after->tv_sec) 
    {
      if (after->tv_usec < before->tv_usec)
        return 0;
      else
        micro = after->tv_usec - before->tv_usec;
    } 
  else 
    {
      micro = MICROSECS_IN_SECOND - before->tv_usec;
      micro += (after->tv_sec - before->tv_sec - 1) * MICROSECS_IN_SECOND;
      micro += after->tv_usec;
    }

  return (micro/MICROSECS_IN_MILLISEC);
}

int
millisec_gt(struct timeval *time1, struct timeval *time2)
{
  assert(time1 != NULL && time2 != NULL);
  assert(time1 != time2);

  /* achu: sigh, where's operator overloading when you need it */

  if ((time1->tv_sec > time2->tv_sec)
      || (time1->tv_sec == time2->tv_sec
          && time1->tv_usec > time2->tv_usec))
    return 1;
  
  return 0;
}

#if HAVE_MMAP && HAVE_MLOCK
void *
Secure_malloc(size_t len)
{
#ifndef MAP_ANONYMOUS
  int fd;
#endif
  void *ptr;

  assert(len > 0);

#ifdef MAP_ANONYMOUS
  if ((ptr = mmap(NULL, len, PROT_READ | PROT_WRITE, 
                  MAP_SHARED | MAP_ANONYMOUS, -1, 0)) == MAP_FAILED)
    err_exit("Secure_malloc: mmap: %s", strerror(errno));
#else
  if ((fd = open("/dev/zero", O_RDWR)) < 0)
    err_exit("Secure_malloc: open: %s", strerror(errno));
  if ((ptr = mmap(NULL, len, PROT_READ | PROT_WRITE,
                  MAP_SHARED, fd, 0)) == MAP_FAILED)
    err_exit("Secure_malloc: mmap: %s", strerror(errno));
#endif

  /* MAP_LOCKED for mmap is only on Linux 2.5.37+ systems.  So we'll
   * just use mlock instead.
   */
  if (mlock(ptr, len) < 0)
    err_exit("Secure_strdup: mlock: %s", strerror(errno));

  memset(ptr, '\0', len);
  return ptr;
}

void
Secure_free(void *ptr, size_t len)
{
  assert(ptr != NULL);
  assert(len > 0);

  /* Clear out any important stuff */
  memset(ptr, '\0', len);

  /* No need to munlock, munmap is sufficient */
  if (munmap(ptr, len) < 0)
    err_exit("Secure_free: munmap: %s", strerror(errno));
}
#else  /* !(HAVE_MMAP && HAVE_MLOCK) */
void *
Secure_malloc(size_t len)
{
  void *ptr;

  assert(len > 0);

  if ((ptr = malloc(len)) == NULL)
    err_exit("Secure_malloc: malloc: %s", strerror(errno));
  memset(ptr, '\0', len);
  return ptr;
}

void
Secure_free(void *ptr, size_t len)
{
  assert(ptr != NULL);
  assert(len > 0);
  free(ptr);
}
#endif /* HAVE_MMAP && HAVE_MLOCK */
