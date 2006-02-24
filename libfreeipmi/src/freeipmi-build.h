/* 
   freeipmi-build.h - includes & defines for libfreeipmi

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _FREEIPMI_BUILD_H
#define	_FREEIPMI_BUILD_H	1

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

#endif /* !HAVE_CONFIG_H */

#if defined(__FreeBSD__) && !defined(EBADMSG)
# define EBADMSG    ENOMSG
#endif

#include "fiid.h"
#include "ipmi-comp-code-spec.h"
#include "ipmi-error.h"
#include "ipmi-locate.h"
#include "ipmi-locate-smbios.h"
#include "ipmi-locate-acpi-spmi.h"
#include "ipmi-locate-pci.h"
#include "ipmi-locate-defaults.h"
#include "rmcp.h"
#include "ipmi-cmd-spec.h"
#include "ipmi-netfn-spec.h"
#include "ipmi-kcs-interface.h"
#include "ipmi-ssif-interface.h"
#include "ipmi-udm.h"
#include "ipmi-ipmb-interface.h"
#include "ipmi-lan-interface.h"
#include "ipmi-lan-interface-udm.h"
#include "ipmi-kcs-interface-udm.h"
#include "ipmi-smic-interface.h"
#include "ipmi-ssif-interface-udm.h"
#include "ipmi-messaging-support-cmds.h"
#include "ipmi-messaging-support-cmds-udm.h"
#include "ipmi-chassis-cmds.h"
#include "ipmi-chassis-cmds-udm.h"
#include "ipmi-device-global-cmds.h"
#include "ipmi-device-global-cmds-udm.h"
#include "ipmi-sdr-repository-cmds.h"
#include "ipmi-sdr-repository-cmds-udm.h"
#include "ipmi-sdr-record-types.h"
#include "ipmi-sensor-types-spec.h"
#include "ipmi-sensor-units-spec.h"
#include "ipmi-sensor-cmds.h"
#include "ipmi-sensor-cmds-udm.h"
#include "ipmi-sel-record-types.h"
#include "ipmi-sel-cmds.h"
#include "ipmi-sel-cmds-udm.h"
#include "ipmi-lan-param-spec.h"
#include "ipmi-lan-cmds.h"
#include "ipmi-lan-cmds-udm.h"
#include "ipmi-serial-modem-param-spec.h"
#include "ipmi-serial-modem-cmds.h"
#include "ipmi-serial-modem-cmds-udm.h"
#include "ipmi-sol-param-spec.h"
#include "ipmi-sol-cmds.h"
#include "ipmi-sol-cmds-udm.h"
#include "ipmi-debug.h"
#include "ipmi-sdr-repository-cache-api.h"
#include "ipmi-sensor-utils.h"
#include "ipmi-bmc-watchdog-timer-cmds.h"
#include "ipmi-pef-param-spec.h"
#include "ipmi-pef-and-alerting-cmds.h"
#include "ipmi-pef-and-alerting-cmds-udm.h"
#include "ipmi-sensor-event-messages.h"
#include "ipmi-sensor-api.h"
#include "ipmi-sel-api.h"

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

#endif /* __FreeBSD__ */

#ifdef __cplusplus
}
#endif

#endif /* freeipmi-build.h */

