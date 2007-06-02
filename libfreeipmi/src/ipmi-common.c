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

#include "ipmi-common.h"

#define IPMI_DPRINTF_MAX_BUF_LEN 65536

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
freeipmi_dprintf(int fd, char *fmt, ...)
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

