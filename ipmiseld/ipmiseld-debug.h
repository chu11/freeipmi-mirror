/*****************************************************************************\
 *  $Id: ipmiseld.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2012-2014 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-559172
 *
 *  This file is part of Ipmiseld, an IPMI SEL syslog logging daemon.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiseld is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiseld is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiseld.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMISELD_DEBUG_H
#define IPMISELD_DEBUG_H

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "ipmiseld.h"

#include "error.h"

#define IPMISELD_DEBUG_BUFFER_LEN 8192

#ifndef NDEBUG

#define IPMISELD_HOST_DEBUG(__msg) \
  do { \
    char __err[IPMISELD_DEBUG_BUFFER_LEN]; \
    int __len; \
    memset (__err, '\0', IPMISELD_DEBUG_BUFFER_LEN); \
    __len = snprintf (__err, \
                      IPMISELD_DEBUG_BUFFER_LEN, \
                      "(%s, %s, %d): hostname = %s; ", \
                      __FILE__, \
                      __FUNCTION__, \
                      __LINE__, \
                      host_data->hostname ? host_data->hostname : "localhost");	\
    if (__len < IPMISELD_DEBUG_BUFFER_LEN) \
      { \
        char *__str; \
        if ((__str = ipmiseld_debug_msg_create __msg)) \
          { \
            strncat (__err, __str, IPMISELD_DEBUG_BUFFER_LEN - __len - 1); \
            free (__str); \
          } \
      } \
    err_debug (__err); \
  } while (0)

#else /* !NDEBUG */

#define IPMISELD_HOST_DEBUG(__msg) \
  do { \
    char __err[IPMISELD_DEBUG_BUFFER_LEN]; \
    int __len; \
    memset (__err, '\0', IPMISELD_DEBUG_BUFFER_LEN); \
    __len = snprintf (__err, \
                      IPMISELD_DEBUG_BUFFER_LEN, \
                      "hostname = %s ; ", \
                      host_data->hostname ? host_data->hostname : "localhost");	\
    if (__len < IPMISELD_DEBUG_BUFFER_LEN) \
      { \
        char *__str; \
        if ((__str = ipmiseld_debug_msg_create __msg)) \
          { \
            strncat (__err, __str, IPMISELD_DEBUG_BUFFER_LEN - __len - 1); \
            free (__str); \
          } \
      } \
    err_debug (__err); \
  } while (0)

#endif /* NDEBUG */

#define IPMISELD_DEBUG(__msg) \
  do { \
    err_debug __msg; \
  } while (0)

/*
 * ipmiseld_debug_msg_create
 *
 * create a buffer and put the a mesage inside it
 *
 * Returns message buffer or NULL on error
 */
char *ipmiseld_debug_msg_create(const char *fmt, ...);

#endif /* IPMISELD_DEBUG_H */
