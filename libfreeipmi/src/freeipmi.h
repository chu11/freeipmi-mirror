/* 
   freeipmi.h - C library interface to IPMI

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#ifndef _FREEIPMI_H
#define	_FREEIPMI_H	1

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <err.h>
#include <signal.h>
#include <errno.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <stdint.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <netdb.h>
#include <getopt.h>
#include <assert.h>
#include <limits.h>
#include <syslog.h>

#ifndef _OS2_
#  if (defined(__GLIBC__) && __GLIBC__ >= 2)
#    include <sys/io.h>
#  elif defined (__OpenBSD__) || defined (__NetBSD__)
#    include <machine/pio.h>/* inb/outb */
#    include <machine/sysarch.h>/* sysarch call */
#  elif defined (__FreeBSD__)
#    include <machine/cpufunc.h>
#    include <machine/sysarch.h>
#  elif defined (PPC)
#    include <asm/io.h>
#  else
#    ifdef _AXP_
#       include <sys/io.h>
#    endif
#  endif   
#endif

#ifdef HAVE_CONFIG_H
# ifdef STDC_HEADERS
#  include <string.h>
#  include <stdarg.h>
# else
#  include <stdint.h>
#  ifndef HAVE_MEMCPY
    static void*
    memcpy (void *dest, const void *src, size_t n)
    {
      while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
      return dest;
    }
#  endif
#  ifndef HAVE_MEMSET
    static void*
    memset (void *s, int c, size_t n)
    {
      while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
      return s;
    }
#  endif
#  ifndef HAVE_STRCHR
   static char*
   strchr (const char* s, int c)
   {
     while (*s != '\0')
       if (*s == (char)c) return s;
       else s++;
     return NULL;
   }
#  endif
# endif

# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif

/* AIX requires this to be the first thing in the file.  */
# ifndef __GNUC__
#  if HAVE_ALLOCA_H
#   include <alloca.h>
#  else
#   ifdef _AIX
#    pragma alloca
#   else
#    ifndef alloca /* predefined by HP cc +Olibcalls */
      char *alloca ();
#    endif
#   endif
#  endif
# endif

# if HAVE_FCNTL_H
#  if defined(__FreeBSD__) && !defined(USE_IOPERM)
#   include <fcntl.h>
#  else
#   include <fcntl.h>
#  endif
# endif

# if TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  if HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   ifdef __FreeBSD__
#    include <sys/time.h>
#   else
#    include <time.h>
#   endif
#  endif
# endif

# if ENABLE_NLS
#  include <libintl.h>
#  define _(Text) gettext (Text)
# else
#  define textdomain(Domain)
#  define _(Text) Text
# endif
# define N_(Text) Text

#else /* HAVE_CONFIG_H */
# include <string.h>
# include <stdarg.h>
# include <unistd.h>
# include <alloca.h>
# include <fcntl.h>
# include <sys/time.h>
# include <time.h>
# include <libintl.h>
#endif /* HAVE_CONFIG_H */

#if defined(__FreeBSD__)
# define _INB(port)  inb (port)
# define _OUTB(data, port)  outb (port, data)
#else
# define _INB(port)  inb (port)
# define _OUTB(data, port)  outb (data, port)
#endif

#if defined(__FreeBSD__) && !defined(EBADMSG)
# define EBADMSG    ENOMSG
#endif

#ifdef FREEIPMI_LIBRARY
# include "xmalloc.h"
#endif

#if  __WORDSIZE == 64
#define FI_64 "%l"
#else
#define FI_64 "%ll"
#endif

#if defined (FREEIPMI_BUILD)
#include "bit-ops.h"
#include "fiid.h"
#include "ipmi-utils.h"
#include "ipmi-error.h"
#if 0 /* TEST */
#include "ipmi-locate.h"
#include "smbios-locate.h"
#include "acpi-spmi-locate.h"
#include "pci-locate.h"
#include "defaults-locate.h"
#endif /* TEST */
#include "rmcp.h"
#include "ipmi-cmd-spec.h"
#include "ipmi-netfn-spec.h"
#if 0 /* TEST */
#include "ipmi-semaphores.h"
#endif /* TEST */
#include "ipmi-interface.h"
#include "ipmi-sessions.h"
#include "ipmi-ipmb-interface.h"
#include "ipmi-lan-interface.h"
#if 0 /* TEST */
#include "ipmi-kcs-interface.h"
#include "ipmi-smic-interface.h"
#include "ipmi-ssif-interface.h"
#endif /* TEST */
#include "ipmi-msg-support-cmds.h"
#if 0 /* TEST */
#include "ipmi-chassis-cmds.h"
#include "ipmi-dev-global-cmds.h"
#include "ipmi-sdr-repo-cmds.h"
#include "ipmi-sdr-record-types.h"
#include "ipmi-sensor-types.h"
#include "ipmi-sensor-cmds.h"
#include "ipmi-sel-record-types.h"
#include "ipmi-sel-cmds.h"
#include "ipmi-lan-param-spec.h"
#include "ipmi-lan-cmds.h"
#include "ipmi-serial-modem-param-spec.h"
#include "ipmi-serial-cmds.h"
#include "ipmi-sol-cmds.h"
#endif /* TEST */
#include "ipmi-debug.h"
#include "ipmi-md2.h"
#include "ipmi-md5.h"
#include "ipmi-sha1.h"
#include "ipmi-hmac.h"
#if 0 /* TEST */
#include "ipmi-sdr-repo-cache.h"
#include "ipmi-sensor-utils.h"
#include "ipmi-watchdog.h"
#include "ipmi-msg-interface-desc.h"
#endif /* TEST */
#include "ipmi-ping.h"
#if 0 /* TEST */
#include "ipmi-pef-param-spec.h"
#include "ipmi-pef-cmds.h"
#include "ipmi-sensor-event-messages.h"
#include "ipmi-sensor-api.h"
#include "ipmi-sel-api.h"
#endif /* TEST */
#ifdef __FreeBSD__
extern void freeipmi_error(int __status, int __errnum,
	const char *__format, ...)
		__attribute__ ((__format__ (__printf__, 3, 4)));
char *freeipmi_strndup(const char *, size_t);
ssize_t freeipmi_getline(char **buf, size_t *bufsize, FILE *fp);

#define error	freeipmi_error
#define strndup	freeipmi_strndup
#define getline	freeipmi_getline

#if __GNUC__
#define _program_name   __progname
#define program_invocation_short_name	__progname
extern char *__progname;
#else
#define _program_name   (NULL)
#define	program_invocation_short_name	(NULL)
#endif /* !__GNUC__ */

/* Replacements for glibc-isms */

#ifdef __GNUC__
/* Duplicate S, returning an identical alloca'd string.  */
#define strdupa(s)                             \
 (__extension__                                \
  ({                                           \
   __const char *__old = (s);                  \
   size_t __len = strlen (__old) + 1;          \
   char *__new = (char *) alloca (__len);      \
   (char *) memcpy (__new, __old, __len);      \
  }))

/* Return an alloca'd copy of at most N bytes of string.  */
#define strndupa(s, n)                         \
 (__extension__                                \
  ({                                           \
   __const char *__old = (s);                  \
   size_t __len = strlen (__old);              \
   char *__new = (char *) alloca (__len + 1);  \
   __new[__len] = '\0';                        \
   (char *) memcpy (__new, __old, __len);      \
  }))

/* Evaluate EXPRESSION, and repeat as long as it returns -1 with `errno'
 *    set to EINTR.  */

#define TEMP_FAILURE_RETRY(expression)         \
 (__extension__                                \
  ({ long int __result;                        \
   do __result = (long int) (expression);      \
   while (__result == -1L && errno == EINTR);  \
  __result; }))
#endif /* __GNUC__ */

#endif /* __FreeBSD__ */
#else
#include <freeipmi/bit-ops.h>
#include <freeipmi/fiid.h>
#include <freeipmi/ipmi-utils.h>
#include <freeipmi/ipmi-error.h>
#if 0 /* TEST */
#include <freeipmi/ipmi-locate.h>
#include <freeipmi/smbios-locate.h>
#include <freeipmi/acpi-spmi-locate.h>
#include <freeipmi/pci-locate.h>
#include <freeipmi/defaults-locate.h>
#endif /* TEST */
#include <freeipmi/rmcp.h>
#include <freeipmi/ipmi-cmd-spec.h>
#include <freeipmi/ipmi-netfn-spec.h>
#if 0
#include <freeipmi/ipmi-semaphores.h>
#endif /* TEST */
#include <freeipmi/ipmi-interface.h>
#include <freeipmi/ipmi-sessions.h>
#include <freeipmi/ipmi-ipmb-interface.h>
#include <freeipmi/ipmi-lan-interface.h>
#if 0 /* TEST */
#include <freeipmi/ipmi-kcs-interface.h>
#include <freeipmi/ipmi-smic-interface.h>
#include <freeipmi/ipmi-ssif-interface.h>
#endif /* TEST */
#include <freeipmi/ipmi-msg-support-cmds.h>
#if 0 /* TEST */
#include <freeipmi/ipmi-chassis-cmds.h>
#include <freeipmi/ipmi-dev-global-cmds.h>
#include <freeipmi/ipmi-sdr-record-types.h>
#include <freeipmi/ipmi-sdr-repo-cmds.h>
#include <freeipmi/ipmi-sensor-cmds.h>
#include <freeipmi/ipmi-sensor-types.h>
#include <freeipmi/ipmi-sel-record-types.h>
#include <freeipmi/ipmi-sel-cmds.h>
#include <freeipmi/ipmi-lan-param-spec.h>
#include <freeipmi/ipmi-lan-cmds.h>
#include <freeipmi/ipmi-serial-modem-param-spec.h>
#include <freeipmi/ipmi-serial-cmds.h>
#include <freeipmi/ipmi-sol-cmds.h>
#endif /* TEST */
#include <freeipmi/ipmi-debug.h>
#if 0 /* TEST */
#include <freeipmi/ipmi-md2.h>
#include <freeipmi/ipmi-md5.h>
#include <freeipmi/ipmi-sha1.h>
#include <freeipmi/ipmi-hmac.h>
#include <freeipmi/ipmi-sdr-repo-cache.h>
#include <freeipmi/ipmi-sensor-utils.h>
#include <freeipmi/ipmi-watchdog.h>
#include <freeipmi/ipmi-msg-interface-desc.h>
#endif /* TEST */
#include <freeipmi/ipmi-ping.h>
#if 0 /* TEST */
#include <freeipmi/ipmi-pef-cmds.h>
#include <freeipmi/ipmi-pef-param-spec.h>
#include <freeipmi/ipmi-sensor-event-messages.h>
#include <freeipmi/ipmi-sensor-api.h>
#include <freeipmi/ipmi-sel-api.h>
#endif /* TEST */
#endif

#ifdef __cplusplus
}
#endif

#endif /* freeipmi.h */

