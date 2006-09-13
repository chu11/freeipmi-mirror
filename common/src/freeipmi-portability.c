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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <errno.h>

#include "freeipmi-portability.h"

#ifndef HAVE_ERROR
/* Replacement for glibc error() */
void
freeipmi_error(int status, int errnum, const char *message, ...)
{
	va_list args;

	fflush(stdout);
	fprintf(stderr, "%s: ", program_invocation_short_name);

	va_start(args, message);
	vfprintf(stderr, message, args);
	va_end(args);

	if (errnum) {
#ifdef HAVE_STRERROR_R
		char buf[256];

		strerror_r(errnum, buf, sizeof(buf));
		fprintf(stderr, ": %s", buf);
#else
		/* XXX strerror is not not thread safe */
		fprintf(stderr, ": %s", strerror(errnum));
#endif
	}

	putc('\n', stderr);
	if (status)
		exit(status);
}
#endif

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
