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
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <argp.h>
#include <assert.h>

#include "ipmi-common.h"
#include "freeipmi/ipmi-messaging-support-cmds.h"

#define IPMI_DPRINTF_MAX_BUF_LEN 65536

int
ipmi_is_root ()
{
  uid_t uid = getuid ();
  if (uid == 0)
    return 1;
  return 0;
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

void
ipmi_disable_coredump(void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit resource_limit;

  if (!getrlimit(RLIMIT_CORE, &resource_limit))
    {
      resource_limit.rlim_cur = 0;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
        perror ("warning: setrlimit()");
    }
#endif /* NDEBUG */
}

/* a k_g key is interpreted as ascii text unless it is prefixed with
   "0x", in which case is it interpreted as hexadecimal */
int
parse_kg(unsigned char *outbuf, int outsz, const char *instr)
{
  char *p, *q;
  int i, j;
  char buf[3] = {0, 0, 0};

  assert(outbuf != NULL);
  assert(instr != NULL);
  assert(outsz == IPMI_MAX_K_G_LENGTH);

  if (strlen(instr) == 0)
    return 0;

  if (strncmp(instr, "0x", 2) == 0) 
    {
      if (strlen(instr) > IPMI_MAX_K_G_LENGTH*2+2)
        return -1;
      p = instr + 2;
      guaranteed_memset(outbuf, 0, IPMI_MAX_K_G_LENGTH);
      for (i = j = 0; i < strlen(p); i+=2, j++)
        {
          if (p[i+1] == '\0')
            return -1;
          buf[0] = p[i]; buf[1] = p[i+1]; buf[2] = 0;
          errno = 0;
          outbuf[j] = strtoul(buf, &q, 16);
          if (errno || (q != buf + 2))
            return -1;
        }
    }
  else
    {
      if (strlen(instr) > IPMI_MAX_K_G_LENGTH)
        return -1;
      guaranteed_memset(outbuf, 0, IPMI_MAX_K_G_LENGTH);
      memcpy(outbuf, instr, strlen(instr));
    }

  return 1;
}

char *
format_kg(char *outstr, int outsz, const unsigned char *k_g)
{
  int i;
  int printable = 1;
  int foundnull = 0;
  char *p;

  assert(outstr != NULL);
  assert(outsz > IPMI_MAX_K_G_LENGTH*2);
  assert(k_g != NULL);

  /* Are there any characters that would prevent printing this as a
     string on a single line? */
  for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
    {
      if (k_g[i] == '\0')
        {
          ++foundnull;
          continue;
        }
      if (!(isgraph(k_g[i]) || k_g[i] == ' ') || foundnull)
        {
          printable = 0;
          break;
        }
    }

  /* print out an entirely null key in hex rather than an empty
     string */
  if (foundnull == IPMI_MAX_K_G_LENGTH)
    printable = 0;

  /* don't print out a key starting with a literal '0x' as a string,
     since parse_kg will try to interpret such strings as hex */
  if (k_g[0] == '0' && k_g[1] == 'x')
    printable = 0;

  if (printable)
    {
      if (outsz < IPMI_MAX_K_G_LENGTH+1)
        return NULL;
      p = outstr;
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
        {
          if (k_g[i] == '\0')
            break;
          p[i] = k_g[i];
        }
      p[i] = 0;
    }
  else
    {
      if (outsz < IPMI_MAX_K_G_LENGTH*2+1)
        return NULL;
      p = outstr;
      p[0] = '0'; p[1] = 'x';
      p+=2;
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++, p+=2)
        sprintf(p, "%02x", k_g[i]);
    }

  return outstr;
}
