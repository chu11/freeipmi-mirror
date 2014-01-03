/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#ifndef FREEIPMI_PORTABILITY_H
#define FREEIPMI_PORTABILITY_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>      /* For FILE definition */
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <math.h>
#include <netdb.h>
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif /* HAVE_ALLOCA_H */
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

/* achu: I guess __func__ is the other macro people use?? */
#ifndef HAVE_FUNCTION_MACRO
#define __FUNCTION__ __func__
#endif

/* achu: not on Solaris */
#ifndef UINT_MAX
#define UINT_MAX 4294967295U
#endif

#if  __WORDSIZE == 64
#define FI_64 "%l"
#else
#define FI_64 "%ll"
#endif

# if ENABLE_NLS
#  include <libintl.h>
#  define _(Text) gettext (Text)
# else
#  define textdomain(Domain)
#  define _(Text) Text
# endif
# define N_(Text) Text

#if !defined(O_SYNC) && defined(O_FSYNC)
#define O_SYNC  O_FSYNC
#endif

/* FreeBSD don't have log2(), exp10() and exp2() */
#ifndef HAVE_LOG2
/* Cygwin has a log2 macro to already handle portability */
#ifndef log2
#define log2(x)         (M_LOG2E * log ((x)))
#endif
#endif
#ifndef HAVE_EXP10
#define exp10(x)        (pow (10.0, (x)))
#endif
#ifndef HAVE_EXP2
#define exp2(x)         (pow (2.0, (x)))
#endif
/* uClibc may not have cbrt() */
#ifndef HAVE_CBRT
#define cbrt(x)         (pow((x), -3.0))
#endif

/* FreeBSD don't have strdupa */
#ifndef strdupa
/* Duplicate S, returning an identical alloca'd string.  */
# define strdupa(s)                             \
  ({                                            \
    const char *__old = (s);                    \
    size_t __len = strlen (__old) + 1;          \
    char *__new = (char *) alloca (__len);      \
    (char *) memcpy (__new, __old, __len);      \
  })
#endif

#ifndef HAVE_MEMCPY
static inline void*
freeipmi_memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n)
    ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return (dest);
}
#define memcpy freeipmi_memcpy
#endif /* HAVE_MEMCPY */

#ifndef HAVE_MEMPCPY
#define mempcpy freeipmi_mempcpy
void *freeipmi_mempcpy (void *to, const void *from, size_t size);
#endif /* HAVE_MEMPCPY */

#ifndef HAVE_MEMSET
static inline void*
freeipmi_memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return (s);
}
#define memset freeipmi_memset
#endif /* HAVE_MEMSET */

#ifndef HAVE_STRCHR
static inline char*
freeipmi_strchr (const char* s, int c)
{
  while (*s != '\0')
    if (*s == (char)c)
      return s;
    else
      s++;
  return (NULL);
}
# define strchr freeipmi_strchr
#endif /* HAVE_STRCHR */

/* FreeBSD don't have strndup() */
#ifndef HAVE_STRNDUP
#define strndup freeipmi_strndup
char *freeipmi_strndup (const char *, size_t);
#endif

#ifndef HAVE_STRCHRNUL
#define strchrnul freeipmi_strchrnul
char *freeipmi_strchrnul (const char *s, int c);
#endif /* !HAVE_STRCHRNUL */

#ifndef HAVE_STRSEP
#define strsep freeipmi_strsep
char *freeipmi_strsep (char **stringp, const char *delim);
#endif /* !HAVE_STRSEP */

#ifndef HAVE_STRISTR
#define stristr freeipmi_stristr
char *freeipmi_stristr (const char *s1, const char *s2);
#endif /* !HAVE_STRISTR */

/* FreeBSD don't have getline() */
#ifndef HAVE_GETLINE
#define getline freeipmi_getline
ssize_t freeipmi_getline (char **buf, size_t *bufsize, FILE *fp);
#endif

#ifndef HAVE_ASPRINTF
#define asprintf freeipmi_asprintf
int freeipmi_asprintf (char **strp, const char *fmt, ...);
#endif

/* achu: timeradd and timersub not in solaris
 *
 * these definitions ripped from sys/time.h on linux.
 */
#ifndef timeradd
# define timeradd(a, b, result)                         \
  do {                                                  \
    (result)->tv_sec = (a)->tv_sec + (b)->tv_sec;       \
    (result)->tv_usec = (a)->tv_usec + (b)->tv_usec;    \
    if ((result)->tv_usec >= 1000000)                   \
      {                                                 \
        ++(result)->tv_sec;                             \
        (result)->tv_usec -= 1000000;                   \
      }                                                 \
  } while (0)
#endif /* timeradd */

#ifndef timersub
# define timersub(a, b, result)                         \
  do {                                                  \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;       \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;    \
    if ((result)->tv_usec < 0) {                        \
      --(result)->tv_sec;                               \
      (result)->tv_usec += 1000000;                     \
    }                                                   \
  } while (0)
#endif /* timersub */

#if !defined(HAVE_FUNC_GETHOSTBYNAME_R_6) && !defined(HAVE_FUNC_GETHOSTBYNAME_R_5)
int freeipmi_gethostbyname_r (const char *name,
                              struct hostent *ret,
                              char *buf,
                              size_t buflen,
                              struct hostent **result,
                              int *h_errnop);
#endif /* !defined(HAVE_FUNC_GETHOSTBYNAME_R_6) && !defined(HAVE_FUNC_GETHOSTBYNAME_R_5) */

#endif /* FREEIPMI_PORTABILITY_H */
