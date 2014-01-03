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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <sys/types.h>
#include <sys/socket.h>

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <netdb.h>
#if HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */

#include "freeipmi-portability.h"

#ifndef HAVE_MEMPCPY
/* Written by Niels Moller <nisse@lysator.liu.se>
 *
 * This file is hereby placed in the public domain.
 */
void *
freeipmi_mempcpy (void *to, const void *from, size_t size)
{
  memcpy (to, from, size);
  return (char *) to + size;
}
#endif

#ifndef HAVE_STRNDUP
/* Replacement for glibc strndup() */
char *
freeipmi_strndup (const char *s, size_t n)
{
  size_t len = strlen (s);
  char *new;

  if (len > n)
    len = n;
  new = (char *)malloc (len + 1);
  if (new == NULL)
    return (NULL);

  new[len] = '\0';
  return (char *)memcpy (new, s, len);
}
#endif

#ifndef HAVE_STRCHRNUL
/* Written by Niels Moller <nisse@lysator.liu.se>
 *
 * This file is hereby placed in the public domain.
 */
char *
freeipmi_strchrnul (const char *s, int c)
{
  const char *p = s;
  while (*p && (*p != c))
    p++;

  return (char *) p;
}
#endif

#ifndef HAVE_STRSEP
/* achu: ripped from wine
 * http://www.winehq.org/pipermail/wine-patches/2001-November/001322.html
 */
char *
freeipmi_strsep (char **stringp, const char *delim)
{
  char *token;

  if (*stringp == NULL)
    {
      /* No more tokens */
      return (NULL);
    }

  token = *stringp;
  while (**stringp != '\0')
    {
      if (strchr (delim, **stringp) != NULL)
        {
          **stringp = '\0';
          (*stringp)++;
          return token;
        }
      (*stringp)++;
    }

  /* There is no other token */
  *stringp = NULL;
  return token;
}
#endif

#ifndef HAVE_STRISTR
/* achu: this is my cheap and quick implementation */
static void
_to_uppercase (char *s)
{
  while ((*s) != '\0')
    {
      if ((*s) >= 'A'
          && (*s) <= 'Z')
        (*s) = (*s) + ('a' - 'A');
      s++;
    }
}

char *
freeipmi_stristr (const char *s1, const char *s2)
{
  char *s1cpy = NULL;
  char *s2cpy = NULL;
  char *ptr;
  char *rv = NULL;

  if (!s1 || !s2)
    goto cleanup;

  if (!(s1cpy = strdup (s1)))
    goto cleanup;

  if (!(s2cpy = strdup (s2)))
    goto cleanup;

  _to_uppercase (s1cpy);
  _to_uppercase (s2cpy);

  if ((ptr = strstr (s1cpy, s2cpy)))
    rv = ((char *)s1 + (ptr - s1cpy));

 cleanup:
  free (s1cpy);
  free (s2cpy);
  return (rv);
}
#endif /* !HAVE_STRISTR */

#ifndef HAVE_GETLINE
/* Replacement for glibc getline */
ssize_t
freeipmi_getline (char **buf, size_t *size, FILE *fp)
{
  ssize_t n = 0;

  if (buf == NULL || size == NULL) {
    errno = EINVAL;
    return (-1);
  }

  if (*buf == NULL || *size == 0) {
    *size = 120;
    *buf = (char *)malloc (*size);
    if (*buf == NULL)
      return (-1);
  }

  for (;;) {
    int ch;

    if (n + 2 > *size) {
      size_t newsize = *size * 2;
      char *newbuf = (char *)realloc (*buf, newsize);

      if (newbuf == NULL)
    return (-1);
      *buf = newbuf;
      *size = newsize;
    }

    ch = fgetc (fp);

    if (ch == -1) {
      (*buf)[n++] = '\0';
      return (-1);
    }

    (*buf)[n++] = ch;

    if (ch == '\n') {
      (*buf)[n++] = '\0';
      return n;
    }
  }
}
#endif

#ifndef HAVE_ASPRINTF
/* achu: ripped from IkiWiki
 * http://ikiwiki.info/bugs/build_in_opensolaris/
 */
int
freeipmi_asprintf (char **strp, const char *fmt, ...)
{
  va_list arg;
  char *str;
  int size;
  int rv;

  va_start (arg, fmt);
  size = vsnprintf (NULL, 0, fmt, arg);
  size++;

  va_start (arg, fmt);
  str = malloc (size);
  if (str == NULL)
    {
      va_end (arg);
      /*
       * Strictly speaking, GNU asprintf doesn't do this,
       * but the caller isn't checking the return value.
       */
      perror ("malloc");
      exit (1);
    }

  rv = vsnprintf (str, size, fmt, arg);
  va_end (arg);

  *strp = str;
  return (rv);
}
#endif

#ifndef HAVE_FUNC_GETHOSTBYNAME_R_6
/*
 * Replacement for 6-argument variant of gethostbyname_r(). This is a
 * slightly modified version of gethostbyname_r() by Enzo Michelangeli
 * from http://www.cygwin.com/ml/cygwin/2004-04/msg00532.html.
 *
 * If we have thread-safe gethostbyname() we just need to protect
 * static thread data from being overwritten by next call to
 * gethostbyname(). So mutex is not required.
 */
int freeipmi_gethostbyname_r (const char *name,
                              struct hostent *ret,
                              char *buf,
                              size_t buflen,
                              struct hostent **result,
                              int *h_errnop)
{
  int hsave;
  struct hostent *ph;
#ifndef HAVE_THREAD_SAFE_GETHOSTBYNAME
  static pthread_mutex_t __mutex = PTHREAD_MUTEX_INITIALIZER;

  pthread_mutex_lock (&__mutex);      /* begin critical area */
#endif
  hsave = h_errno;
  ph = gethostbyname (name);
  *h_errnop = h_errno;       /* copy h_errno to *h_herrnop */

  if (ph == NULL) {
    *result = NULL;
  } else {
    char **p, **q;
    char *pbuf;
    int nbytes=0;
    int naddr=0, naliases=0;

    /* determine if we have enough space in buf */

    /* count how many addresses */
    for (p = ph->h_addr_list; *p != 0; p++) {
      nbytes += ph->h_length;               /* addresses */
      nbytes += sizeof (*p);               /* pointers */
      naddr++;
    }
    nbytes += sizeof (*p);           /* one more for the terminating NULL */

    /* count how many aliases, and total length of strings */

    for (p = ph->h_aliases; *p != 0; p++) {
      nbytes += (strlen (*p)+1);              /* aliases */
      nbytes += sizeof (*p);                /* pointers */
      naliases++;
    }
    nbytes += sizeof (*p);           /* one more for the terminating NULL */

    /* here nbytes is the number of bytes required in buffer */
    /* as a terminator must be there, the minimum value is ph->h_length */
    if(nbytes > buflen) {
      *result = NULL;
#ifndef HAVE_THREAD_SAFE_GETHOSTBYNAME
      pthread_mutex_unlock (&__mutex);              /* end critical area */
#endif
      return ERANGE;               /* not enough space in buf!! */
    }

    /* There is enough space. Now we need to do a deep copy! */
    /* Allocation in buffer:
       from [0] to [(naddr-1) * sizeof (*p)]:
       pointers to addresses
       at [naddr * sizeof (*p)]:
       NULL
       from [(naddr+1) * sizeof (*p)] to [(naddr+naliases) * sizeof (*p)] :
       pointers to aliases
       at [(naddr+naliases+1) * sizeof (*p)]:
       NULL
       then naddr addresses (fixed length), and naliases aliases (asciiz).
    */

    *ret = *ph;             /* copy whole structure (not its address!) */

    /* copy addresses */
    q = (char **)buf;           /* pointer to pointers area (type: char **) */
    ret->h_addr_list = q;           /* update pointer to address list */
    pbuf = buf + ((naddr+naliases+2)*sizeof (*p));           /* skip that area */
    for (p = ph->h_addr_list; *p != 0; p++) {
      memcpy (pbuf, *p, ph->h_length);              /* copy address bytes */
      *q++ = pbuf;               /* the pointer is the one inside buf... */
      pbuf += ph->h_length;               /* advance pbuf */
    }
    *q++ = NULL;           /* address list terminator */

    /* copy aliases */

    ret->h_aliases = q;           /* update pointer to aliases list */
    for (p = ph->h_aliases; *p != 0; p++) {
      strcpy (pbuf, *p);              /* copy alias strings */
      *q++ = pbuf;               /* the pointer is the one inside buf... */
      pbuf += strlen (*p);              /* advance pbuf */
      *pbuf++ = 0;               /* string terminator */
    }
    *q++ = NULL;           /* terminator */

    strcpy (pbuf, ph->h_name);          /* copy alias strings */
    ret->h_name = pbuf;
    pbuf += strlen (ph->h_name);          /* advance pbuf */
    *pbuf++ = 0;           /* string terminator */

    *result = ret;            /* and let *result point to structure */

  }
  h_errno = hsave;        /* restore h_errno */

#ifndef HAVE_THREAD_SAFE_GETHOSTBYNAME
  pthread_mutex_unlock (&__mutex);      /* end critical area */
#endif

  return (*result == NULL);
}
#endif /* HAVE_FUNC_GETHOSTBYNAME_R_6 */
