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

#include <stdio.h>

#include "xmalloc.h"

/* Prototypes for functions defined here.  */
#if defined (__STDC__) && __STDC__
static __VOID *fixup_null_alloc (size_t n);
#endif

/* Exit value when the requested amount of memory is not available.
   The caller may set it to some other value.  */
int xmalloc_exit_failure = EXIT_FAILURE;

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
      perror ("malloc");
      exit (xmalloc_exit_failure);
    }

  return p;
}

/* Allocate N bytes of memory dynamically, with error checking.  */
__VOID *
xmalloc (n)
     size_t n;
{
  __VOID *p;

  p = malloc (n);
  if (p == 0)
    p = fixup_null_alloc (n);
  return p;
}

/* Allocate memory for N elements of S bytes, with error checking.  */
__VOID *
xcalloc (n, s)
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
xrealloc (p, n)
     __VOID *p;
     size_t n;
{
  if (p == 0)
    return xmalloc (n);
  p = realloc (p, n);
  if (p == 0)
    p = fixup_null_alloc (n);
  return p;
}

/* Make a copy of a string in a newly allocated block of memory. */
char *
xstrdup (str)
     char *str;
{
  __VOID *p;

  p = xmalloc (strlen (str) + 1);
  strcpy (p, str);
  return p;
}

void
xfree (p)
     __VOID *p;
{
  if (p)
    free (p);
  p = NULL;
}
