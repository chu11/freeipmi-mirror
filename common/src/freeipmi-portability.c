/* 
   libfreeipmi - C library interface to FreeIPMI

   Copyright (C) 2002, 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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
#include <pthread.h>

#include "freeipmi-portability.h"

#ifndef HAVE_STRNDUP
/* Replacement for glibc strndup() */
char *
freeipmi_strndup(const char *s, size_t n)
{
	size_t len = strlen(s);
	char *new;
	
	if (len > n)
		len = n;
	new = (char *)malloc(len + 1);
	if (new == NULL)
		return NULL;

	new[len] = '\0';
	return (char *)memcpy(new, s, len);
}
#endif

#ifndef HAVE_GETLINE
/* Replacement for glibc getline */
ssize_t
freeipmi_getline(char **buf, size_t *size, FILE *fp)
{
	ssize_t n = 0;

	if (buf == NULL || size == NULL) {
		errno = EINVAL;
		return -1;
	}

	if (*buf == NULL || *size == 0) {
		*size = 120;
		*buf = (char *)malloc(*size);
		if (*buf == NULL)
			return -1;
	}

	for (;;) {
		int ch;

		if (n + 2 > *size) {
			size_t newsize = *size * 2;
			char *newbuf = (char *)realloc(*buf, newsize);

			if (newbuf == NULL)
				return -1;
			*buf = newbuf;
			*size = newsize;
		}

		ch = fgetc(fp);

		if (ch == -1) {
			(*buf)[n++] = '\0';
			return -1;
		}

		(*buf)[n++] = ch;

		if (ch == '\n') {
			(*buf)[n++] = '\0';
			return n;
		}
	}
}
#endif

/*
 * Replacement for 6-argument variant of gethostbyname_r(). This is a
 * slightly modified version of gethostbyname_r() by Enzo Michelangeli
 * from http://www.cygwin.com/ml/cygwin/2004-04/msg00532.html.
 *
 * If we have thread-safe gethostbyname() we just need to protect
 * static thread data from being overwritten by next call to
 * gethostbyname(). So mutex is not required.
 */
#ifdef LOCAL_GETHOSTBYNAME_R
int freeipmi_gethostbyname_r(const char *name,
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

	pthread_mutex_lock(&__mutex); /* begin critical area */
#endif
	hsave = h_errno;
	ph = gethostbyname(name);
	*h_errnop = h_errno; /* copy h_errno to *h_herrnop */

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
			nbytes += ph->h_length; /* addresses */
			nbytes += sizeof(*p); /* pointers */
			naddr++;
		}
		nbytes += sizeof(*p); /* one more for the terminating NULL */

		/* count how many aliases, and total length of strings */

		for (p = ph->h_aliases; *p != 0; p++) {
			nbytes += (strlen(*p)+1); /* aliases */
			nbytes += sizeof(*p);  /* pointers */
			naliases++;
		}
		nbytes += sizeof(*p); /* one more for the terminating NULL */

		/* here nbytes is the number of bytes required in buffer */
		/* as a terminator must be there, the minimum value is ph->h_length */
		if(nbytes > buflen) {
			*result = NULL;
#ifndef HAVE_THREAD_SAFE_GETHOSTBYNAME
			pthread_mutex_unlock(&__mutex); /* end critical area */
#endif
			return ERANGE; /* not enough space in buf!! */
		}

		/* There is enough space. Now we need to do a deep copy! */
		/* Allocation in buffer:
		 from [0] to [(naddr-1) * sizeof(*p)]:
			 pointers to addresses
		 at [naddr * sizeof(*p)]:
			 NULL
		 from [(naddr+1) * sizeof(*p)] to [(naddr+naliases) * sizeof(*p)] :
			 pointers to aliases
		 at [(naddr+naliases+1) * sizeof(*p)]:
			 NULL
		 then naddr addresses (fixed length), and naliases aliases (asciiz).
		*/

		*ret = *ph;   /* copy whole structure (not its address!) */

		/* copy addresses */
		q = (char **)buf; /* pointer to pointers area (type: char **) */
		ret->h_addr_list = q; /* update pointer to address list */
		pbuf = buf + ((naddr+naliases+2)*sizeof(*p)); /* skip that area */
		for (p = ph->h_addr_list; *p != 0; p++) {
			memcpy(pbuf, *p, ph->h_length); /* copy address bytes */
			*q++ = pbuf; /* the pointer is the one inside buf... */
			pbuf += ph->h_length; /* advance pbuf */
		}
		*q++ = NULL; /* address list terminator */

		/* copy aliases */

		ret->h_aliases = q; /* update pointer to aliases list */
		for (p = ph->h_aliases; *p != 0; p++) {
			strcpy(pbuf, *p); /* copy alias strings */
			*q++ = pbuf; /* the pointer is the one inside buf... */
			pbuf += strlen(*p); /* advance pbuf */
			*pbuf++ = 0; /* string terminator */
		}
		*q++ = NULL; /* terminator */

		strcpy(pbuf, ph->h_name); /* copy alias strings */
		ret->h_name = pbuf;
		pbuf += strlen(ph->h_name); /* advance pbuf */
		*pbuf++ = 0; /* string terminator */

		*result = ret;  /* and let *result point to structure */

	}
	h_errno = hsave;  /* restore h_errno */

#ifndef HAVE_THREAD_SAFE_GETHOSTBYNAME
	pthread_mutex_unlock(&__mutex); /* end critical area */
#endif

	return (*result == NULL);
}
#endif
