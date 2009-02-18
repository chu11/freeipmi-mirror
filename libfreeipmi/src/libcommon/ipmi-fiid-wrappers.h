/* 
   Copyright (C) 2003-2009 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

*/

#ifndef _IPMI_FIID_WRAPPERS_H
#define	_IPMI_FIID_WRAPPERS_H 1

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/fiid/fiid.h"

#define FIID_WRAPPER_STR_MAX_LEN 4096

#if defined (IPMI_TRACE)
#define __FIID_TRACE                                                 \
do {                                                                 \
  extern int errno;                                                  \
  int __save_errno = errno;                                          \
  char __errnostr[ERR_WRAPPER_STR_MAX_LEN];                          \
  memset (__errnostr, '\0', ERR_WRAPPER_STR_MAX_LEN);                \
  strerror_r(__save_errno, __errnostr, ERR_WRAPPER_STR_MAX_LEN);     \
  fprintf (stderr,                                                   \
           "%s: %d: %s: errno %s (%d)\n",                            \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                  \
           __errnostr, __save_errno);                                \
  fflush (stderr);                                                   \
  errno = __save_errno;                                              \
} while (0)

#define __FIID_ERRNUM_TRACE(___errnum)                             \
do {                                                               \
  fprintf (stderr,                                                 \
	   "%s: %d: %s: error = %s (%d)\n",                        \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                \
	   fiid_strerror(___errnum),                               \
           ___errnum);                                             \
  fflush (stderr);                                                 \
} while (0)

#define __FIID_OBJ_TRACE(___obj)                                   \
do {                                                               \
  int32_t __obj_errnum = fiid_obj_errnum((___obj));                \
  __FIID_ERRNUM_TRACE(__obj_errnum);                               \
} while (0)

#define __FIID_OBJ_NO_DATA_TRACE(__field)                          \
do {                                                               \
  fprintf (stderr,                                                 \
	   "%s: %d: %s: %s: no data\n",                            \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                \
           __field);                                               \
  fflush (stderr);                                                 \
} while (0)

#else
#define __FIID_TRACE
#define __FIID_OBJ_TRACE(___obj)
#define __FIID_OBJ_NO_DATA_TRACE(__field)
#endif /* IPMI_TRACE */

#define __FIID_ERRNUM_SET_ERRNO(___errnum)              \
do {                                                    \
  if ((___errnum) == FIID_ERR_SUCCESS)                  \
    errno = 0;                                          \
  else if ((___errnum) == FIID_ERR_OUT_OF_MEMORY)       \
    errno = ENOMEM;                                     \
  else if ((___errnum) == FIID_ERR_OVERFLOW)            \
    errno = ENOSPC;                                     \
  else                                                  \
    errno = EINVAL;                                     \
} while (0)

#define __FIID_OBJ_SET_ERRNO(___obj)                    \
do {                                                    \
  int32_t ___objerrnum = fiid_obj_errnum((___obj));     \
  __FIID_ERRNUM_SET_ERRNO(___objerrnum);                \
} while (0)

#ifdef __cplusplus
}
#endif

#endif /* ipmi-fiid-wrappers.h */
