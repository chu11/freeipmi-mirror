/*****************************************************************************\
 *  $Id: secure.c,v 1.8 2010-02-08 22:02:30 chu11 Exp $
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
#if HAVE_MMAP
#include <sys/mman.h>
#endif /* HAVE_MMAP */
#include <assert.h>
#include <errno.h>

#include "secure.h"

/* From David Wheeler's Secure Programming Guide */
void *secure_memset (void *s, int c, size_t n)
{
  volatile char *p;

  if (!s || !n)
    return (NULL);

  p = s;
  while (n--)
    *p++=c;

  return (s);
}

void *
secure_malloc (size_t len)
{
  void *ptr;

  assert (len);

#if defined(MAP_ANONYMOUS) && defined(MAP_LOCK) && HAVE_MMAP
  if ((ptr = mmap (NULL,
                   len,
                   PROT_READ | PROT_WRITE,
                   MAP_SHARED | MAP_ANONYMOUS | MAP_LOCKED,
                   -1,
                   0)) == MAP_FAILED)
    return (NULL);
#elif defined(MAP_ANONYMOUS) && !defined(MAP_LOCK)
  if ((ptr = mmap (NULL,
                   len,
                   PROT_READ | PROT_WRITE,
                   MAP_SHARED | MAP_ANONYMOUS,
                   -1,
                   0)) == MAP_FAILED)
    return (NULL);
  if (mlock (ptr, len) < 0)
    {
      munmap (ptr, len);
      return (NULL);
    }
#else /* !defined(MAP_ANONYMOUS) */
  if (!(ptr = malloc (len)))
    return (NULL);
#endif /* !defined(MAP_ANONYMOUS) */

#if 0
  /* The following case could be implemented, however, we don't compile
   * it because it can cause fd leaks.
   */
  if ((fd = open ("/dev/zero", O_RDWR)) < 0)
    return (NULL);
  if ((ptr = mmap (NULL,
                   len,
                   PROT_READ | PROT_WRITE,
                   MAP_SHARED,
                   -1,
                   0)) == MAP_FAILED)
    {
      /* ignore potential error, this is error path */
      close (fd);
      return (NULL);
    }
  if (mlock (ptr, len) < 0)
    {
      /* ignore potential error, this is error path */
      munmap (ptr, len);
      /* ignore potential error, this is error path */
      close (fd);
      return (NULL);
    }
#endif /* 0 */

  secure_memset (ptr, '\0', len);
  return (ptr);
}

void
secure_free (void *ptr, size_t len)
{
  assert (len);

  if (ptr)
    {
      secure_memset (ptr, '\0', len);
#if defined(MAP_ANONYMOUS) && defined(MAP_LOCK) && HAVE_MMAP
      /* ignore potential error, void return func */
      munmap (ptr, len);
#elif defined(MAP_ANONYMOUS) && !defined(MAP_LOCK)
      /* ignore potential error, void return func */
      /* munlock is not necessary, munmap is sufficient */
      munmap (ptr, len);
#else /* !defined(MAP_ANONYMOUS) */
      free (ptr);
#endif /* !defined(MAP_ANONYMOUS) */
    }
}
