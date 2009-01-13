/*****************************************************************************\
 *  $Id: ipmi_monitoring_debug.h,v 1.9 2009-01-13 01:02:21 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2009 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
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
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMI_MONITORING_DEBUG_H
#define _IPMI_MONITORING_DEBUG_H

#include <freeipmi/freeipmi.h>

#include "ipmi_monitoring.h"

#define IPMI_MONITORING_DEBUG_ERROR_BUFLEN 4096

#define IPMI_MONITORING_DEBUG(__msg) \
  do { \
    char __err[IPMI_MONITORING_DEBUG_ERROR_BUFLEN]; \
    int __len; \
    memset(__err, '\0', IPMI_MONITORING_DEBUG_ERROR_BUFLEN); \
    __len = snprintf(__err, \
                     IPMI_MONITORING_DEBUG_ERROR_BUFLEN, \
                     "(%s, %s, %d): ", \
                     __FILE__, \
                     __FUNCTION__, \
                     __LINE__); \
    if (__len < IPMI_MONITORING_DEBUG_ERROR_BUFLEN) \
      { \
        char *__str; \
        if ((__str = __debug_msg_create __msg)) \
          { \
            strncat(__err, __str, IPMI_MONITORING_DEBUG_ERROR_BUFLEN - __len - 1); \
            free(__str); \
          } \
      } \
      ipmi_monitoring_debug(__err); \
  } while(0)

void ipmi_monitoring_debug(const char *fmt, ...);

char * __debug_msg_create(const char *fmt, ...);

#endif /* _IPMI_MONITORING_DEBUG_H */
