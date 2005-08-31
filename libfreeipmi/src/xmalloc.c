/* xmalloc.c -- malloc with out of memory checking
   Copyright (C) 1990, 91, 92, 93, 94, 95, 96, 99 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include <stdio.h>

#ifdef STDC_HEADERS
#include <string.h>
#else
# include <sys/types.h>

# ifndef HAVE_MEMCPY
static void*
memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return dest;
}
# endif
# ifndef HAVE_MEMSET
static void*
memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return s;
}
# endif
#endif

#include <errno.h>
#include <syslog.h>
#include "xmalloc.h"

/* Prototypes for functions defined here.  */
#if defined (__STDC__) && __STDC__
static __VOID *fixup_null_alloc (size_t n);
#endif

static __VOID *
fixup_null_alloc (n)
     size_t n;
{
  __VOID *p;

  p = 0;
  if (n == 0)
    p = malloc ((size_t) 1);
  if (p == 0)
    {
      syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), _("Memory exhausted"));
      errno = ENOMEM;
    }
  else 
    memset (p, 0, 1);
  
  return p;
}

/* Allocate N bytes of memory dynamically, with error checking.  */
__VOID *
ipmi_xmalloc (n)
     size_t n;
{
  __VOID *p;

  p = malloc (n);
  if (p == 0)
    p = fixup_null_alloc (n);
  else 
    memset (p, 0, n);
  
  return p;
}

/* Allocate memory for N elements of S bytes, with error checking.  */
__VOID *
ipmi_xcalloc (n, s)
     size_t n, s;
{
  __VOID *p;

  p = calloc (n, s);
  if (p == 0)
    p = fixup_null_alloc (n);
  return p;
}

/* Change the size of an allocated block of memory P to N bytes,
   with error checking.
   If P is NULL, run xmalloc.  */
__VOID *
ipmi_xrealloc (p, n)
     __VOID *p;
     size_t n;
{
  if (p == 0)
    return ipmi_xmalloc (n);
  p = realloc (p, n);
  if (p == 0)
    p = fixup_null_alloc (n);
  return p;
}

/* Make a copy of a string in a newly allocated block of memory. */
char *
ipmi_xstrdup (str)
     char *str;
{
  __VOID *p;

  p = ipmi_xmalloc (strlen (str) + 1);
  strcpy (p, str);
  return p;
}

void
ipmi_xfree (__VOID *p)
{
  if (p)
    {
      free (p);
      p = 0;
    }
}
