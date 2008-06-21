/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

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
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

#define IPMI_DEBUG_MAX_BUF_LEN        65536
#define IPMI_DEBUG_MAX_PKT_LEN        65536
#define IPMI_DEBUG_CHAR_PER_LINE          8
#define IPMI_DEBUG_DEFAULT_FD STDERR_FILENO

static int
_write(int fd, void *buf, size_t n)
{
  /* chu: by Chris Dunlap <dunlap6 at llnl dot gov> */
  size_t nleft;
  ssize_t nwritten;
  unsigned char *p;

  p = buf;
  nleft = n;
  while (nleft > 0)
    {
      if ((nwritten = write (fd, p, nleft)) < 0)
        {
          if (errno == EINTR)
            continue;
          else
            return (-1);
        }
      nleft -= nwritten;
      p += nwritten;
    }
  return (n);
}

int
ipmi_debug_dprintf(int fd, char *fmt, ...)
{
  va_list ap;
  int len, rv;
  char buf[IPMI_DEBUG_MAX_BUF_LEN];

  va_start(ap, fmt);
  len = vsnprintf(buf, IPMI_DEBUG_MAX_BUF_LEN, fmt, ap);
  rv = _write(fd, buf, len);
  va_end(ap);

  return rv;
}

int8_t
ipmi_debug_set_prefix(char *buf, unsigned int buflen, const char *prefix)
{
  assert(buf && buflen > 3);

  memset(buf, '\0', buflen);
  if (prefix)
    {
      strncpy(buf, prefix, buflen);
      buf[buflen - 1] = '\0'; /* strncpy may not null terminate */
      buf[buflen - 2] = '\0'; /* guaranteed space for ' ' */
      buf[buflen - 3] = '\0'; /* guaranteed space for ':' */
      strcat(buf, ": ");
    }

  return (0);
}

int8_t
ipmi_debug_output_str(int fd, const char *prefix, const char *str)
{
  /* achu: Yeah, I know this is slow.  Figure out something better
   * later.
   */
  if (str)
    {
      char *ptr = (char *)str;

      if (prefix)
        IPMI_DEBUG_DPRINTF((fd, "%s", prefix));
      while (*ptr != '\0')
        {
          if (*ptr == '\n')
            {
              IPMI_DEBUG_DPRINTF((fd, "%c", *ptr++));
              if (prefix)
                IPMI_DEBUG_DPRINTF((fd, "%s", prefix));
            }
          else
            IPMI_DEBUG_DPRINTF((fd, "%c", *ptr++));
        }
      IPMI_DEBUG_DPRINTF((fd, "\n"));
    }

  return 0;
} 

int8_t
ipmi_debug_output_byte_array(int fd, const char *prefix, uint8_t *buf, uint32_t buf_len)
{
  uint32_t count = 0;

  assert(buf);

  while (count < buf_len)
    {
      int i = 0;
      if (prefix)
        IPMI_DEBUG_DPRINTF ((fd, "%s", prefix));
      IPMI_DEBUG_DPRINTF ((fd, "[ "));
      while (count < buf_len && i < IPMI_DEBUG_CHAR_PER_LINE)
	{
	  IPMI_DEBUG_DPRINTF ((fd, "%02Xh ", buf[count++]));
	  i++;
	}
      IPMI_DEBUG_DPRINTF ((fd, "]\n"));
    }

  return 0;
}

