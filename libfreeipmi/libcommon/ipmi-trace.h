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

#ifndef IPMI_TRACE_H
#define IPMI_TRACE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/fiid/fiid.h"

#include "ipmi-fiid-util.h"

#define ERR_WRAPPER_STR_MAX_LEN 4096

#if defined (IPMI_TRACE)
#define TRACE_MSG_OUT(__msgtracestr, __msgtracenum)       \
  do {                                                  \
    fprintf (stderr,                                    \
             "%s: %d: %s: error '%s' (%d)\n",           \
             __FILE__, __LINE__, __FUNCTION__,          \
             __msgtracestr, __msgtracenum);             \
    fflush (stderr);                                    \
  } while (0)

#define TRACE_ERRNO_OUT(__errno_orig)                                     \
  do {                                                                  \
    extern int errno;                                                   \
    int __save_errno = __errno_orig;                                    \
    char __errnostr[ERR_WRAPPER_STR_MAX_LEN];                           \
    memset (__errnostr, '\0', ERR_WRAPPER_STR_MAX_LEN);                 \
    strerror_r (__save_errno, __errnostr, ERR_WRAPPER_STR_MAX_LEN);     \
    fprintf (stderr,                                                    \
             "%s: %d: %s: errno '%s' (%d)\n",                           \
             __FILE__, __LINE__, __FUNCTION__,                          \
             __errnostr, __save_errno);                                 \
    fflush (stderr);                                                    \
    __errno_orig = __save_errno;                                        \
  } while (0)
#else /* !IPMI_TRACE */
#define TRACE_MSG_OUT(__msgtracestr, __msgtracenum)
#define TRACE_ERRNO_OUT(__errno_orig)
#endif /* !IPMI_TRACE */

#define ERR_TRACE(__str, __num)                 \
  do {                                          \
    TRACE_MSG_OUT (__str, __num);               \
  } while (0)

#define SET_ERRNO(__errno)                      \
  do {                                          \
    errno = (__errno);                          \
    TRACE_ERRNO_OUT (errno);                    \
  } while (0)

#define ERRNO_TRACE(__errno)                    \
  do {                                          \
    TRACE_ERRNO_OUT (__errno);                  \
  } while (0)

#define FIID_OBJECT_ERROR_TO_ERRNO(__obj)                                   \
  do {                                                                      \
    set_errno_by_fiid_object ((__obj));                                     \
    TRACE_MSG_OUT (fiid_obj_errormsg ((__obj)), fiid_obj_errnum ((__obj))); \
  } while (0)

#define FIID_ITERATOR_ERROR_TO_ERRNO(__iter)                                            \
  do {                                                                                  \
    set_errno_by_fiid_iterator ((__iter));                                              \
    TRACE_MSG_OUT (fiid_iterator_errormsg ((__iter)), fiid_iterator_errnum ((__iter))); \
  } while (0)

#endif /* IPMI_TRACE_H */
