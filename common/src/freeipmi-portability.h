/* 
   freeipmi-portability.h - portability includes & defines for libfreeipmi

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _FREEIPMI_PORTABILITY_H
#define	_FREEIPMI_PORTABILITY_H	1

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef STDC_HEADERS
#ifndef HAVE_MEMCPY
static void*
memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return dest;
}
#endif /* HAVE_MEMCPY */
#  ifndef HAVE_MEMSET
static void*
memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return s;
}
#endif /* HAVE_MEMSET */
#ifndef HAVE_STRCHR
static char*
strchr (const char* s, int c)
{
  while (*s != '\0')
    if (*s == (char)c) return s;
    else s++;
  return NULL;
}
#endif /* HAVE_STRCHR */
#endif /* STDC_HEADERS */

#if defined(__FreeBSD__) && !defined(EBADMSG)
# define EBADMSG    ENOMSG
#endif

#ifdef __FreeBSD__
extern void freeipmi_error(int __status, int __errnum,
	const char *__format, ...)
		__attribute__ ((__format__ (__printf__, 3, 4)));
char *freeipmi_strndup(const char *, size_t);
ssize_t freeipmi_getline(char **buf, size_t *bufsize, FILE *fp);

#define error	freeipmi_error
#define strndup	freeipmi_strndup
#define getline	freeipmi_getline

#endif /* __FreeBSD__ */

#ifdef __cplusplus
}
#endif

#endif /* freeipmi-portability.h */

