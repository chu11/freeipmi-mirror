/*****************************************************************************\
 *  $Id: ipmi_monitoring_debug.c,v 1.2 2007-02-16 20:23:31 chu11 Exp $
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
#include <syslog.h>
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
  if (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_STDOUT)
    fprintf(stdout, "%s\n", errbuf);
  if (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_STDERR)
    fprintf(stderr, "%s\n", errbuf);
  if (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_SYSLOG)
    syslog(LOG_DEBUG, "%s", errbuf);
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

void
ipmi_monitoring_inband_dump(char *prefix, fiid_obj_t obj_cmd)
{
  int fd;

  assert(fiid_obj_valid(obj_cmd));

  if (!(_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS))
    return;

  if (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_STDOUT)
    fd = STDOUT_FILENO;
  else if (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_STDERR)
    fd = STDERR_FILENO;
  else
    return;

  if (ipmi_obj_dump_perror(fd, prefix, NULL, NULL, obj_cmd) < 0)
    IPMI_MONITORING_DEBUG(("ipmi_obj_dump: %s", strerror(errno)));
}

void
ipmi_monitoring_outofband_dump(char *prefix, 
                               char *hdr,
                               fiid_obj_t obj_rmcp_hdr,
                               fiid_obj_t obj_lan_session_hdr,
                               fiid_obj_t obj_msg_hdr,
                               fiid_obj_t obj_cmd,
                               fiid_obj_t obj_msg_trlr)
{
  char *fmt =
    "================================================\n"
    "%s\n"
    "================================================\n";
  char *rmcp_hdr =
    "RMCP Header:\n"
    "------------";
  char *lan_session_hdr =
    "IPMI LAN Session Header:\n"
    "-----------------------";
  char *msg_hdr =
    "IPMI Message Header:\n"
    "--------------------";
  char *cmd_hdr =
    "IPMI Command Data:\n"
    "------------------";
  char *trlr_hdr =
    "IPMI Trailer:\n"
    "--------------";
  int fd;

  if (!(_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_IPMI_PACKETS))
    return;

  if (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_STDOUT)
    fd = STDOUT_FILENO;
  else if (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_DEBUG_STDERR)
    fd = STDERR_FILENO;
  else
    return;
  
  if (hdr)
    {
      char hdrbuf[1024];
      int len;

      if ((len = snprintf(hdrbuf, 1024, fmt, hdr)) < 0)
        {
          IPMI_MONITORING_DEBUG(("snprintf"));
          return;
        }
 
      if (len >= 1024)
        {
          IPMI_MONITORING_DEBUG(("snprintf truncation: len = %d", len));
          return;
        }

      if (fd_write_n(fd, hdrbuf, len) < 0)
        {
          IPMI_MONITORING_DEBUG(("fd_write_n: %s", strerror(errno)));
          return;
        }
    }

  if (obj_rmcp_hdr)
    {
      if (ipmi_obj_dump_perror(fd, prefix, rmcp_hdr, NULL, obj_rmcp_hdr) < 0)
        IPMI_MONITORING_DEBUG(("ipmi_obj_dump_perror: %s", strerror(errno)));
    }

  if (obj_lan_session_hdr)
    {
      if (ipmi_obj_dump_perror(fd, prefix, lan_session_hdr, NULL, obj_lan_session_hdr) < 0)
        IPMI_MONITORING_DEBUG(("ipmi_obj_dump_perror: %s", strerror(errno)));
    }

  if (obj_msg_hdr)
    {
      if (ipmi_obj_dump_perror(fd, prefix, msg_hdr, NULL, obj_msg_hdr) < 0)
        IPMI_MONITORING_DEBUG(("ipmi_obj_dump_perror: %s", strerror(errno)));
    }

  if (obj_cmd)
    {
      if (ipmi_obj_dump_perror(fd, prefix, cmd_hdr, NULL, obj_cmd) < 0)
        IPMI_MONITORING_DEBUG(("ipmi_obj_dump_perror: %s", strerror(errno)));
    }

  if (obj_msg_trlr)
    {
      if (ipmi_obj_dump_perror(fd, prefix, trlr_hdr, NULL, obj_msg_trlr) < 0)
        IPMI_MONITORING_DEBUG(("ipmi_obj_dump_perror: %s", strerror(errno)));
    }
}
