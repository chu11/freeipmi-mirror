/* xmalloc.h -- malloc with out of memory checking
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  */


#ifndef _XMALLOC_H
#define	_XMALLOC_H	1

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>

#if __STDC__
# define __VOID void
#else
# define __VOID char
#endif

#if STDC_HEADERS
# include <stdlib.h>
#else
__VOID *calloc ();
__VOID *malloc ();
__VOID *realloc ();
void free ();
#endif

/* Prototypes for functions defined here.  */
#if defined (__STDC__) && __STDC__
__VOID *xmalloc (size_t n);
__VOID *xcalloc (size_t n, size_t s);
__VOID *xrealloc (__VOID *p, size_t n);
char *xstrdup (char *p);
#define xfree(p)          \
  if (p)		  \
  {			  \
    free (p);		  \
    p = NULL;		  \
  }
#endif

#ifdef __cplusplus
}
#endif
#endif /* xmalloc.h */

