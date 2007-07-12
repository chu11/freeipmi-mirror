/*****************************************************************************\
 *  $Id: ipmi_monitoring_debug.c,v 1.3.8.1 2007-07-12 18:19:04 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <stdarg.h>
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* !HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_debug.h"

#include "fd.h"

extern uint32_t _ipmi_monitoring_flags;

static void
_debug(const char *fmt, va_list ap)
{
  char errbuf[IPMI_MONITORING_DEBUG_ERROR_BUFLEN];

  assert(fmt);

  vsnprintf(errbuf, IPMI_MONITORING_DEBUG_ERROR_BUFLEN, fmt, ap);
  if (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG)
    fprintf(stderr, "%s\n", errbuf);
}

void 
ipmi_monitoring_debug(const char *fmt, ...)
{
  va_list ap;

  assert(fmt);

  va_start(ap, fmt);
  _debug(fmt, ap);
  va_end(ap);
}

char * 
__debug_msg_create(const char *fmt, ...)
{
  char *buffer;
  va_list ap;

  assert(fmt);

  if (!(buffer = malloc(IPMI_MONITORING_DEBUG_ERROR_BUFLEN)))
    return NULL;

  va_start(ap, fmt);
  vsnprintf(buffer, IPMI_MONITORING_DEBUG_ERROR_BUFLEN, fmt, ap);
  va_end(ap);

  return buffer;
}
