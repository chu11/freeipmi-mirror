/*****************************************************************************\
 *  $Id: ipmiconsole_debug.h,v 1.13 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiconsole is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiconsole is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMICONSOLE_DEBUG_H
#define IPMICONSOLE_DEBUG_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

#define IPMICONSOLE_DEBUG_FILENAME     "ipmiconsole_debug"

#define IPMICONSOLE_DEBUG_ERROR_BUFLEN 4096

#define IPMICONSOLE_DEBUG(__msg)                                                \
  do {                                                                          \
    char __err[IPMICONSOLE_DEBUG_ERROR_BUFLEN];                                 \
    int __len;                                                                  \
    memset (__err, '\0', IPMICONSOLE_DEBUG_ERROR_BUFLEN);                       \
    __len = snprintf (__err,                                                    \
                      IPMICONSOLE_DEBUG_ERROR_BUFLEN,                           \
                      "(%s, %s, %d): ",                                         \
                      __FILE__,                                                 \
                      __FUNCTION__,                                             \
                      __LINE__);                                                \
    if (__len < IPMICONSOLE_DEBUG_ERROR_BUFLEN)                                 \
      {                                                                         \
        char *__str;                                                            \
        if ((__str = __debug_msg_create __msg))                                 \
          {                                                                     \
            strncat (__err, __str, IPMICONSOLE_DEBUG_ERROR_BUFLEN - __len - 1); \
            free (__str);                                                       \
          }                                                                     \
      }                                                                         \
    ipmiconsole_debug (__err);                                                  \
  } while(0)

#define IPMICONSOLE_CTX_DEBUG(__c, __msg)                                       \
  do {                                                                          \
    char __err[IPMICONSOLE_DEBUG_ERROR_BUFLEN];                                 \
    int __len;                                                                  \
    memset (__err, '\0', IPMICONSOLE_DEBUG_ERROR_BUFLEN);                       \
    __len = snprintf (__err,                                                    \
                      IPMICONSOLE_DEBUG_ERROR_BUFLEN,                           \
                      "(%s, %s, %d): "                                          \
                      "hostname=%s; "                                           \
                      "protocol_state=%Xh: ",                                   \
                      __FILE__,                                                 \
                      __FUNCTION__,                                             \
                      __LINE__,                                                 \
                      (__c)->config.hostname,                                   \
                      (__c)->session.protocol_state);                           \
    if (__len < IPMICONSOLE_DEBUG_ERROR_BUFLEN)                                 \
      {                                                                         \
        char *__str;                                                            \
        if ((__str = __debug_msg_create __msg))                                 \
          {                                                                     \
            strncat (__err, __str, IPMICONSOLE_DEBUG_ERROR_BUFLEN - __len - 1); \
            free (__str);                                                       \
          }                                                                     \
      }                                                                         \
    ipmiconsole_ctx_debug ((__c), __err);                                       \
  } while(0)

int ipmiconsole_debug_setup (uint32_t debug_flags);

void ipmiconsole_debug_cleanup (void);

void ipmiconsole_debug (const char *fmt, ...);

void ipmiconsole_ctx_debug (ipmiconsole_ctx_t c, const char *fmt, ...);

char * __debug_msg_create (const char *fmt, ...);

#endif /* IPMICONSOLE_DEBUG_H */
