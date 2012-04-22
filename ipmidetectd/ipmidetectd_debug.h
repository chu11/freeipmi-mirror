/*****************************************************************************\
 *  $Id: debug.h,v 1.10 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2012 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-228523
 *
 *  This file is part of Ipmidetect, tools and libraries for detecting
 *  IPMI nodes in a cluster. For details, see http://www.llnl.gov/linux/.
 *
 *  The code in this file began with the code in the Cerebro project.
 *  See below for original copyright information.
 * 
 *  Ipmidetect is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmidetect is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmidetect.  If not, see <http://www.gnu.org/licenses/>.
 *****************************************************************************
 *  Copyright (C) 2005 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>.
 *  UCRL-CODE-155989 All rights reserved.
 *
 *  This file is part of Cerebro, a collection of cluster monitoring
 *  tools and libraries.  For details, see
 *  <http://www.llnl.gov/linux/cerebro/>.
 *
 *  Cerebro is free software; you can redistribute it and/or modify it under
 *  the terms of the GNU General Public License as published by the Free
 *  Software Foundation; either version 2 of the License, or (at your option)
 *  any later version.
 *
 *  Cerebro is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 *  details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Cerebro.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMIDETECTD_DEBUG_H
#define IPMIDETECTD_DEBUG_H

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "error.h"

#define IPMIDETECTD_DEBUG_BUFFER_LEN 8192

#define IPMIDETECTD_DEBUG_MSG_CREATE(__msg) \
    char __err[IPMIDETECTD_DEBUG_BUFFER_LEN]; \
    int __len; \
    \
    memset(__err, '\0', IPMIDETECTD_DEBUG_BUFFER_LEN); \
    \
    __len = snprintf(__err, \
                     IPMIDETECTD_DEBUG_BUFFER_LEN, \
                     "(%s, %s, %d): ", \
                     __FILE__, \
                     __FUNCTION__, \
                     __LINE__); \
    \
    if (__len < IPMIDETECTD_DEBUG_BUFFER_LEN) \
      { \
        char *__str; \
        if ((__str = _ipmidetect_debug_msg_create __msg)) \
          { \
            strncat(__err, __str, IPMIDETECTD_DEBUG_BUFFER_LEN - __len - 1); \
            __len += strlen(__str); \
            free(__str); \
          } \
      }

/*
 * _ipmidetect_debug_msg_create
 *
 * create a buffer and put the a mesage inside it
 *
 * Returns message buffer or NULL on error
 */
char *_ipmidetect_debug_msg_create(const char *fmt, ...);

#ifndef NDEBUG

#define IPMIDETECTD_DEBUG(__msg) \
    do { \
      IPMIDETECTD_DEBUG_MSG_CREATE(__msg) \
      err_debug(__err); \
    } while(0)

#define IPMIDETECTD_OUTPUT(__msg) \
    do { \
      IPMIDETECTD_DEBUG_MSG_CREATE(__msg) \
      err_output(__err); \
    } while(0)

#define IPMIDETECTD_EXIT(__msg) \
    do { \
      IPMIDETECTD_DEBUG_MSG_CREATE(__msg) \
      err_exit(__err); \
    } while(0)
   
#else /* NDEBUG */

#define IPMIDETECTD_DEBUG(__msg)

#define IPMIDETECTD_OUTPUT(__msg) \
    do { \
      err_output __msg; \
    } while(0)

#define IPMIDETECTD_EXIT(__msg) \
    do { \
      err_exit __msg; \
    } while(0)

#endif /* NDEBUG */

#endif /* IPMIDETECTD_DEBUG_H */
