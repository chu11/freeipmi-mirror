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

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
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
#include <errno.h>

#include "freeipmi/util/ipmi-timestamp-util.h"
#include "freeipmi/spec/ipmi-timestamp-spec.h"

#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

#define IPMI_TIMESTAMP_FLAG_MASK \
  (IPMI_TIMESTAMP_FLAG_ABBREVIATE \
   | IPMI_TIMESTAMP_FLAG_UTC_TO_LOCALTIME \
   | IPMI_TIMESTAMP_FLAG_LOCALTIME_TO_UTC)

int
ipmi_timestamp_string (uint32_t timestamp,
		       int utc_offset,
		       unsigned int flags,
		       const char *format,
		       char *buf,
		       unsigned int buflen)
{
  struct tm tm;
  time_t t;

  if ((flags & ~IPMI_TIMESTAMP_FLAG_MASK) || !buf || !buflen)
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  /* Timestamp Special Cases */

  if (timestamp == IPMI_TIMESTAMP_UNSPECIFIED)
    {
      if (flags & IPMI_TIMESTAMP_FLAG_ABBREVIATE)
	snprintf (buf, buflen, "Unspec.");
      else
	snprintf (buf, buflen, "Unspecified");
      return (0);
    }

  if (IPMI_TIMESTAMP_POST_INIT (timestamp))
    {
      if (flags & IPMI_TIMESTAMP_FLAG_ABBREVIATE)
	snprintf (buf, buflen, "PostInit");
      else
	snprintf (buf, buflen, "Post-Init %u s", timestamp);
      return (0);
    }

  /* Posix says individual calls need not clear/set all portions of
   * 'struct tm', thus passing 'struct tm' between functions could
   * have issues.  So we need to memset.
   */
  memset (&tm, '\0', sizeof (struct tm));

  t = timestamp;

  if (utc_offset)
    t += utc_offset;

  if (flags & IPMI_TIMESTAMP_FLAG_UTC_TO_LOCALTIME)
    localtime_r (&t, &tm);
  else if (flags & IPMI_TIMESTAMP_FLAG_LOCALTIME_TO_UTC)
    {
      /* In order for this to be threadsafe, we won't modify the TZ
       * environment variable.  We'll calculate the offset manually.
       *
       * XXX: This may be called alot via tools like ipmi-sel, should
       * find some way to cache this or something if we care about
       * performance later on.
       */
      struct tm gmtm;
      struct tm localtm;
      time_t gmt, localt;
      time_t offset;
      time_t utc_timestamp;

      gmtime_r (&t, &gmtm);
      localtime_r (&t, &localtm);

      gmt = mktime (&gmtm);
      localt = mktime (&localtm);

      /* Notes:
       * localtime = UTC + offset
       * UTC = localtime - offset
       * soo ...
       */
      offset = localt - gmt;

      utc_timestamp = t - offset;

      gmtime_r (&utc_timestamp, &tm);
    }
  else
    gmtime_r (&t, &tm);

  if (format)
    strftime (buf, buflen, format, &tm);
  else
    strftime (buf, buflen, "%m/%d/%Y - %H:%M:%S", &tm);

  return (0);
}
