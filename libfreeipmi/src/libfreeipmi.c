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

#include "freeipmi.h"

#ifdef __FreeBSD__

#include <readline/readline.h>

/* Replacement for glibc error() */
void
freeipmi_error(int status, int errnum, const char *message, ...)
{
	va_list args;

	fflush(stdout);
	if (_program_name)
		fprintf(stderr, "%s: ", _program_name);

	va_start(args, message);
	vfprintf(stderr, message, args);
	va_end(args);

	if (errnum)
		/* XXX strerror is not not thread safe */
		fprintf(stderr, ": %s", strerror(errnum));
	putc('\n', stderr);
	fflush (stderr);
	if (status)
		exit(status);
}

/* Replacement for glibc strndup() */
char *
freeipmi_strndup(const char *s, size_t n)
{
	/* XXX strlen -> strnlen */
	size_t len = strlen(s);
	char *new = (char *)malloc(len + 1);

	if (new == NULL)
		return NULL;

	new[len] = '\0';
	return (char *)memcpy(new, s, len);
}

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

#endif /* __FreeBSD__ */
