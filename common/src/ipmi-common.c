/*
argp-common.c: common work for argp for all freeipmi tools.
Copyright (C) 2005 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <errno.h>
#include <error.h>
#include <argp.h>

#include "ipmi-common.h"

#define IPMI_DPRINTF_MAX_BUF_LEN 65536

int
ipmi_is_root ()
{
  uid_t uid = getuid ();
  if (uid == 0)
    return 1;
  return 0;
}

void 
ipmi_error (fiid_obj_t obj_cmd, uint8_t netfn, const char *s)
{
  char errmsg[IPMI_ERR_STR_MAX_LEN] = { 0 };
  uint64_t cmd;
  int32_t len;
  int8_t rv;

  if (!fiid_obj_valid(obj_cmd))
    return;
  
  if ((rv = fiid_obj_field_lookup (obj_cmd, "cmd")) < 0)
    return;

  if (!rv)
    {
      errno = EINVAL;
      return;
    }

  if ((len = fiid_obj_field_len (obj_cmd, "cmd")) < 0)
    return;

  if (!len)
    {
      errno = EINVAL;
      return;
    }

  if (ipmi_strerror_cmd_r (obj_cmd, netfn, errmsg, IPMI_ERR_STR_MAX_LEN) < 0)
    return;
  
  if (fiid_obj_get(obj_cmd, "cmd", &cmd) < 0)
    return;

  fprintf (stderr, 
	   "%s%s" "ipmi command %02Xh: %s\n", 
	   (s ? s : ""), 
	   (s ? ": " : ""), 
           (uint8_t)cmd,
	   errmsg);
}

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
ipmi_dprintf(int fd, char *fmt, ...)
{
  va_list ap;
  int len, rv;
  char buf[IPMI_DPRINTF_MAX_BUF_LEN];

  va_start(ap, fmt);
  len = vsnprintf(buf, IPMI_DPRINTF_MAX_BUF_LEN, fmt, ap);
  rv = _write(fd, buf, len);
  va_end(ap);

  return rv;
}

/* From David Wheeler's Secure Programming Guide */
void *guaranteed_memset(void *s, int c, size_t n)
{
  volatile char *p = s;

  if (!s || !n)
    return NULL;

  while (n--)
    *p++=c;

  return s;
}
