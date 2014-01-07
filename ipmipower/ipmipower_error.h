/*****************************************************************************\
 *  $Id: ipmipower_error.h,v 1.4 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmipower is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmipower is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/
/*****************************************************************************\
 *  $Id: ipmipower_error.h,v 1.4 2010-02-08 22:02:31 chu11 Exp $
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

#ifndef IPMIPOWER_ERROR_H
#define IPMIPOWER_ERROR_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include "ipmipower.h"

#define IPMIPOWER_ERROR_STDERR 0x00000001
#define IPMIPOWER_ERROR_SYSLOG 0x00000002

#define IPMIPOWER_ERROR_BUFLEN 4096

#define IPMIPOWER_ERROR(__msg)                                                  \
  do {                                                                          \
    char __err[IPMIPOWER_ERROR_BUFLEN];                                         \
    int __len = 0;                                                              \
    memset (__err, '\0', IPMIPOWER_ERROR_BUFLEN);                               \
    if (cmd_args.common_args.debug)                                             \
      __len = snprintf (__err,                                                  \
                        IPMIPOWER_ERROR_BUFLEN,                                 \
                        "(%s, %s, %d): ",                                       \
                        __FILE__,                                               \
                        __FUNCTION__,                                           \
                        __LINE__);                                              \
    if (__len < IPMIPOWER_ERROR_BUFLEN)                                         \
      {                                                                         \
        char *__str;                                                            \
        if ((__str = __error_msg_create __msg))                                 \
          {                                                                     \
            strncat (__err, __str, IPMIPOWER_ERROR_BUFLEN - __len - 1);         \
            free (__str);                                                       \
          }                                                                     \
      }                                                                         \
    ipmipower_error (__err);                                                    \
  } while(0)

#define IPMIPOWER_DEBUG(__msg)                                                  \
  do {                                                                          \
    char __err[IPMIPOWER_ERROR_BUFLEN];                                         \
    int __len = 0;                                                              \
    if (cmd_args.common_args.debug)                                             \
      {                                                                         \
        memset (__err, '\0', IPMIPOWER_ERROR_BUFLEN);                           \
        __len = snprintf (__err,                                                \
                          IPMIPOWER_ERROR_BUFLEN,                               \
                          "(%s, %s, %d): ",                                     \
                          __FILE__,                                             \
                          __FUNCTION__,                                         \
                          __LINE__);                                            \
        if (__len < IPMIPOWER_ERROR_BUFLEN)                                     \
          {                                                                     \
            char *__str;                                                        \
            if ((__str = __error_msg_create __msg))                             \
              {                                                                 \
                strncat (__err, __str, IPMIPOWER_ERROR_BUFLEN - __len - 1);     \
                free (__str);                                                   \
              }                                                                 \
          }                                                                     \
        ipmipower_debug (__err);                                                \
      }                                                                         \
  } while(0)

int ipmipower_error_setup (unsigned int error_flags);

void ipmipower_error (const char *fmt, ...);

void ipmipower_debug (const char *fmt, ...);

char * __error_msg_create (const char *fmt, ...);

#endif /* IPMIPOWER_ERROR_H */
