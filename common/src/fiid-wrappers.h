/* 
   fiid-wrappers.h - FreeIPMI Interface Definition wrappers

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#ifndef _FIID_WRAPPERS_H
#define	_FIID_WRAPPERS_H 1

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>
#include <errno.h>

#include "freeipmi/fiid.h"

#define FIID_WRAPPER_STR_MAX_LEN 4096

#if defined (IPMI_SYSLOG)
#define __FIID_SYSLOG                                                   \
do {                                                                    \
  extern int errno;                                                     \
  int save_errno = errno;                                               \
  char errstr[FIID_WRAPPER_STR_MAX_LEN];                                \
  snprintf (errstr, FIID_WRAPPER_STR_MAX_LEN,                           \
	    "%s: %d: %s: errno (%d): expression failed",                \
	    __FILE__,                                                   \
	    __LINE__,                                                   \
	    __PRETTY_FUNCTION__,                                        \
	    save_errno);                                                \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), errstr);         \
  errno = save_errno;                                                   \
} while (0)

#define __FIID_OBJ_SYSLOG(___obj)                                  \
do {                                                               \
  int32_t __obj_errnum = fiid_obj_errnum((___obj));                \
  char errstr[FIID_WRAPPER_STR_MAX_LEN];                           \
  snprintf (errstr, FIID_WRAPPER_STR_MAX_LEN,                      \
	    "%s: %d: %s: error = %s",                              \
	    __FILE__,                                              \
	    __LINE__,                                              \
	    __PRETTY_FUNCTION__,                                   \
	    fiid_strerror(__obj_errnum));                          \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), errstr);    \
} while (0)

#define __FIID_ITER_SYSLOG(___iter)                                \
do {                                                               \
  int32_t __iter_errnum = fiid_iterator_errnum((___iter));         \
  char errstr[FIID_WRAPPER_STR_MAX_LEN];                           \
  snprintf (errstr, FIID_WRAPPER_STR_MAX_LEN,                      \
	    "%s: %d: %s: error = %s",                              \
	    __FILE__,                                              \
	    __LINE__,                                              \
	    __PRETTY_FUNCTION__,                                   \
	    fiid_strerror(__iter_errnum));                         \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), errstr);    \
} while (0)
#else
#define __FIID_SYSLOG
#define __FIID_OBJ_SYSLOG(___obj)
#define __FIID_ITER_SYSLOG(___iter)
#endif /* IPMI_SYSLOG */

#if defined (IPMI_TRACE)
#define __FIID_TRACE                                                    \
do {                                                                    \
  extern int errno;                                                     \
  int save_errno = errno;                                               \
  fprintf (stderr,                                                      \
           "%s: %d: %s: errno (%d): expression failed\n",               \
	   __FILE__,                                                    \
           __LINE__,                                                    \
	   __PRETTY_FUNCTION__,                                         \
	   save_errno);                                                 \
  fflush (stderr);                                                      \
  errno = save_errno;                                                   \
} while (0)

#define __FIID_OBJ_TRACE(___obj)                                   \
do {                                                               \
  int32_t __obj_errnum = fiid_obj_errnum((___obj));                \
  fprintf (stderr,                                                 \
	   "%s: %d: %s: error = %s\n",                             \
	   __FILE__,                                               \
	   __LINE__,                                               \
	   __PRETTY_FUNCTION__,                                    \
	   fiid_strerror(__obj_errnum));                           \
  fflush (stderr);                                                 \
} while (0)

#define __FIID_ITER_TRACE(___iter)                                 \
do {                                                               \
  int32_t __iter_errnum = fiid_iterator_errnum((___iter));         \
  fprintf (stderr,                                                 \
	   "%s: %d: %s: error = %s\n",                             \
	   __FILE__,                                               \
	   __LINE__,                                               \
	   __PRETTY_FUNCTION__,                                    \
	   fiid_strerror(__iter_errnum));                          \
  fflush (stderr);                                                 \
} while (0)

#else
#define __FIID_TRACE
#define __FIID_OBJ_TRACE(___obj)
#define __FIID_ITER_TRACE(___iter)
#endif /* IPMI_TRACE */

#define __FIID_OBJ_SET_ERRNO(___obj)            \
do {                                            \
  int32_t __errnum = fiid_obj_errnum((___obj)); \
  if (__errnum == FIID_ERR_SUCCESS)             \
    errno = 0;                                  \
  else if (__errnum == FIID_ERR_OUTMEM)         \
    errno = ENOMEM;                             \
  else if (__errnum == FIID_ERR_OVERFLOW)       \
    errno = ENOSPC;                             \
  else                                          \
    errno = EINVAL;                             \
} while (0)

#define __FIID_ITER_SET_ERRNO(___iter)                \
do {                                                  \
  int32_t __errnum = fiid_iterator_errnum((___iter)); \
  if (__errnum == FIID_ERR_SUCCESS)                   \
    errno = 0;                                        \
  else if (__errnum == FIID_ERR_OUTMEM)               \
    errno = ENOMEM;                                   \
  else if (__errnum == FIID_ERR_OVERFLOW)             \
    errno = ENOSPC;                                   \
  else                                                \
    errno = EINVAL;                                   \
} while (0)

#define __FIID_OBJ_SET_UDM_ERRNUM(___obj)          \
do {                                               \
  int32_t __errnum = fiid_obj_errnum((___obj));    \
  if (__errnum == FIID_ERR_SUCCESS)                \
    dev->errnum = IPMI_ERR_SUCCESS;                \
  else if (__errnum == FIID_ERR_OUTMEM)            \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;          \
  else                                             \
    dev->errnum = IPMI_ERR_INTERNAL_LIBRARY_ERROR; \
} while (0)

/* EINVAL -> INTERNAL_LIBRARY_ERROR b/c bad inputs should be directly returned
 * to the user as IPMI_ERR_INVALID_PARAMETERS
 */
#define __ERRNO_TO_UDM_ERRNUM                      \
do {                                               \
  if (errno == 0)                                  \
    dev->errnum = IPMI_ERR_SUCCESS;                \
  else if (errno == ENOMEM)                        \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;          \
  else if (errno == ENODEV)                        \
    dev->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;   \
  else if (errno == EINVAL)                        \
    dev->errnum = IPMI_ERR_INTERNAL_LIBRARY_ERROR; \
  else                                             \
    dev->errnum = IPMI_ERR_INTERNAL_ERROR;         \
} while (0)

#define FIID_TEMPLATE_LEN(__len, __tmpl)                    \
do {                                                        \
  if (((__len) = fiid_template_len ((__tmpl))) < 0)         \
    {                                                       \
      __FIID_SYSLOG;                                        \
      __FIID_TRACE;                                         \
      return (-1);                                          \
    }                                                       \
} while (0)

#define FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)              \
do {                                                        \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)   \
    {                                                       \
      __FIID_SYSLOG;                                        \
      __FIID_TRACE;                                         \
      return (-1);                                          \
    }                                                       \
} while (0)

#define FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)      \
do {                                                        \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)   \
    {                                                       \
      __FIID_SYSLOG;                                        \
      __FIID_TRACE;                                         \
      goto cleanup;                                         \
    }                                                       \
} while (0)

#define UDM_FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)          \
do {                                                        \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)   \
    {                                                       \
      __FIID_SYSLOG;                                        \
      __FIID_TRACE;                                         \
      __ERRNO_TO_UDM_ERRNUM;                                \
      return (-1);                                          \
    }                                                       \
} while (0)

#define UDM_FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)  \
do {                                                        \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)   \
    {                                                       \
      __FIID_SYSLOG;                                        \
      __FIID_TRACE;                                         \
      __ERRNO_TO_UDM_ERRNUM;                                \
      goto cleanup;                                         \
    }                                                       \
} while (0)

#define FIID_TEMPLATE_FIELD_START(__len, __tmpl, __field)                    \
do {                                                                         \
  if (((__len) = fiid_template_field_start ((__tmpl), (__field))) < 0)       \
    {                                                                        \
      __FIID_SYSLOG;                                                         \
      __FIID_TRACE;                                                          \
      return (-1);                                                           \
    }                                                                        \
} while (0)

#define FIID_TEMPLATE_FIELD_START_BYTES(__len, __tmpl, __field)              \
do {                                                                         \
  if (((__len) = fiid_template_field_start_bytes ((__tmpl), (__field))) < 0) \
    {                                                                        \
      __FIID_SYSLOG;                                                         \
      __FIID_TRACE;                                                          \
      return (-1);                                                           \
    }                                                                        \
} while (0)

#define FIID_TEMPLATE_FIELD_START_BYTES_CLEANUP(__len, __tmpl, __field)      \
do {                                                                         \
  if (((__len) = fiid_template_field_start_bytes ((__tmpl), (__field))) < 0) \
    {                                                                        \
      __FIID_SYSLOG;                                                         \
      __FIID_TRACE;                                                          \
      goto cleanup;                                                          \
    }                                                                        \
} while (0)

#define FIID_TEMPLATE_FIELD_LEN(__len, __tmpl, __field)                    \
do {                                                                       \
  if (((__len) = fiid_template_field_len ((__tmpl), (__field))) < 0)       \
    {                                                                      \
      __FIID_SYSLOG;                                                       \
      __FIID_TRACE;                                                        \
      return (-1);                                                         \
    }                                                                      \
} while (0)

#define FIID_TEMPLATE_FIELD_LEN_BYTES(__len, __tmpl, __field)              \
do {                                                                       \
  if (((__len) = fiid_template_field_len_bytes ((__tmpl), (__field))) < 0) \
    {                                                                      \
      __FIID_SYSLOG;                                                       \
      __FIID_TRACE;                                                        \
      return (-1);                                                         \
    }                                                                      \
} while (0)

#define FIID_TEMPLATE_FIELD_LEN_BYTES_CLEANUP(__len, __tmpl, __field)      \
do {                                                                       \
  if (((__len) = fiid_template_field_len_bytes ((__tmpl), (__field))) < 0) \
    {                                                                      \
      __FIID_SYSLOG;                                                       \
      __FIID_TRACE;                                                        \
      goto cleanup;                                                        \
    }                                                                      \
} while (0)

#define FIID_TEMPLATE_BLOCK_LEN(__len, __tmpl, __field_start, __field_end)                      \
do {                                                                                            \
  if (((__len) = fiid_template_block_len ((__tmpl), (__field_start), (__field_end))) < 0)       \
    {                                                                                           \
      __FIID_SYSLOG;                                                                            \
      __FIID_TRACE;                                                                             \
      return (-1);                                                                              \
    }                                                                                           \
} while (0)

#define FIID_TEMPLATE_BLOCK_LEN_BYTES(__len, __tmpl, __field_start, __field_end)                \
do {                                                                                            \
  if (((__len) = fiid_template_block_len_bytes ((__tmpl), (__field_start), (__field_end))) < 0) \
    {                                                                                           \
      __FIID_SYSLOG;                                                                            \
      __FIID_TRACE;                                                                             \
      return (-1);                                                                              \
    }                                                                                           \
} while (0)

#define FIID_TEMPLATE_BLOCK_LEN_BYTES_CLEANUP(__len, __tmpl, __field_start, __field_end)        \
do {                                                                                            \
  if (((__len) = fiid_template_block_len_bytes ((__tmpl), (__field_start), (__field_end))) < 0) \
    {                                                                                           \
      __FIID_SYSLOG;                                                                            \
      __FIID_TRACE;                                                                             \
      goto cleanup;                                                                             \
    }                                                                                           \
} while (0)

#define FIID_TEMPLATE_COMPARE(__tmpl1, __tmpl2)                      \
do {                                                                 \
    int __ret;                                                       \
    if ((__ret = fiid_template_compare ((__tmpl1), (__tmpl2))) < 0)  \
      {                                                              \
        __FIID_SYSLOG;                                               \
        __FIID_TRACE;                                                \
        return (-1);                                                 \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
        __FIID_SYSLOG;                                               \
        __FIID_TRACE;                                                \
	return (-1);                                                 \
      }                                                              \
} while (0)

#define FIID_TEMPLATE_COMPARE_CLEANUP(__tmpl1, __tmpl2)              \
do {                                                                 \
    int __ret;                                                       \
    if ((__ret = fiid_template_compare ((__tmpl1), (__tmpl2))) < 0)  \
      {                                                              \
        __FIID_SYSLOG;                                               \
        __FIID_TRACE;                                                \
        goto cleanup;                                                \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
        __FIID_SYSLOG;                                               \
        __FIID_TRACE;                                                \
	goto cleanup;                                                \
      }                                                              \
} while (0)

#define FIID_TEMPLATE_FREE_NO_RETURN(__tmpl)   \
do {                                           \
  if ((__tmpl))                                \
    fiid_template_free((__tmpl));              \
} while (0)

#define UDM_FIID_TEMPLATE_FREE_NO_RETURN(__tmpl)   FIID_TEMPLATE_FREE_NO_RETURN(__tmpl)

#define FIID_OBJ_CREATE(__obj, __tmpl)          \
do {                                            \
  if (!((__obj) = fiid_obj_create(__tmpl)))     \
    {                                           \
      __FIID_SYSLOG;                            \
      __FIID_TRACE;                             \
      return (-1);                              \
    }                                           \
} while (0)

#define FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)  \
do {                                            \
  if (!((__obj) = fiid_obj_create(__tmpl)))     \
    {                                           \
      __FIID_SYSLOG;                            \
      __FIID_TRACE;                             \
      goto cleanup;                             \
    }                                           \
} while (0)

#define UDM_FIID_OBJ_CREATE(__obj, __tmpl)          \
do {                                                \
  if (!((__obj) = fiid_obj_create(__tmpl)))         \
    {                                               \
      __FIID_SYSLOG;                                \
      __FIID_TRACE;                                 \
      dev->errnum = IPMI_ERR_OUT_OF_MEMORY;         \
      return (-1);                                  \
    }                                               \
} while (0)

#define UDM_FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)  \
do {                                                \
  if (!((__obj) = fiid_obj_create(__tmpl)))         \
    {                                               \
      __FIID_SYSLOG;                                \
      __FIID_TRACE;                                 \
      dev->errnum = IPMI_ERR_OUT_OF_MEMORY;         \
      goto cleanup;                                 \
    }                                               \
} while (0)

#define FIID_OBJ_DESTROY(__obj)                \
do {                                           \
 if ((__obj))                                  \
   {                                           \
     if (fiid_obj_destroy((__obj)) < 0)        \
       {                                       \
         __FIID_OBJ_SYSLOG((__obj));           \
         __FIID_OBJ_TRACE((__obj));            \
         __FIID_OBJ_SET_ERRNO((__obj));        \
         return (-1);                          \
       }                                       \
   }                                           \
} while (0)

#define FIID_OBJ_DESTROY_NO_RETURN(__obj)      \
do {                                           \
  if ((__obj))                                 \
    {                                          \
      fiid_obj_destroy((__obj));               \
      (__obj) = NULL;                          \
    }                                          \
} while (0)

#define UDM_FIID_OBJ_DESTROY(__obj)            \
do {                                           \
 if ((__obj))                                  \
   {                                           \
     if (fiid_obj_destroy((__obj)) < 0)        \
       {                                       \
         __FIID_OBJ_SYSLOG((__obj));           \
         __FIID_OBJ_TRACE((__obj));            \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));   \
         return (-1);                          \
       }                                       \
   }                                           \
} while (0)

#define UDM_FIID_OBJ_DESTROY_NO_RETURN(__obj)      FIID_OBJ_DESTROY_NO_RETURN(__obj)

#define FIID_OBJ_DUP(__obj_dest, __obj_src)             \
do {                                                    \
     if (!((__obj_dest) = fiid_obj_dup((__obj_src))))   \
       {                                                \
         __FIID_OBJ_SYSLOG((__obj_src));                \
         __FIID_OBJ_TRACE((__obj_src));                 \
         __FIID_OBJ_SET_ERRNO((__obj_src));             \
         return (-1);                                   \
       }                                                \
} while (0)

#define FIID_OBJ_DUP_CLEANUP(__obj_dest, __obj_src)     \
do {                                                    \
     if (!((__obj_dest) = fiid_obj_dup((__obj_src))))   \
       {                                                \
         __FIID_OBJ_SYSLOG((__obj_src));                \
         __FIID_OBJ_TRACE((__obj_src));                 \
         __FIID_OBJ_SET_ERRNO((__obj_src));             \
         goto cleanup;                                  \
       }                                                \
} while (0)

#define FIID_OBJ_LEN(__len, __obj)                       \
do {                                                     \
    if (((__len) = fiid_obj_len ((__obj))) < 0)          \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_LEN_BYTES(__len, __obj)                 \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_LEN_BYTES_CLEANUP(__len, __obj)         \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         goto cleanup;                                   \
      }                                                  \
} while (0)

#define UDM_FIID_OBJ_LEN_BYTES(__len, __obj)             \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));             \
         return (-1);                                    \
      }                                                  \
} while (0)

#define UDM_FIID_OBJ_LEN_BYTES_CLEANUP(__len, __obj)     \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));             \
         goto cleanup;                                   \
      }                                                  \
} while (0)

#define FIID_OBJ_FIELD_LEN(__len, __obj, __field)                        \
do {                                                                     \
    if (((__len) = fiid_obj_field_len ((__obj), (__field))) < 0)         \
      {                                                                  \
         __FIID_OBJ_SYSLOG((__obj));                                     \
         __FIID_OBJ_TRACE((__obj));                                      \
         __FIID_OBJ_SET_ERRNO((__obj));                                  \
         return (-1);                                                    \
      }                                                                  \
} while (0)

#define FIID_OBJ_FIELD_LEN_BYTES(__len, __obj, __field)                  \
do {                                                                     \
    if (((__len) = fiid_obj_field_len_bytes ((__obj), (__field))) < 0)   \
      {                                                                  \
         __FIID_OBJ_SYSLOG((__obj));                                     \
         __FIID_OBJ_TRACE((__obj));                                      \
         __FIID_OBJ_SET_ERRNO((__obj));                                  \
         return (-1);                                                    \
      }                                                                  \
} while (0)

#define FIID_OBJ_FIELD_LEN_BYTES_CLEANUP(__len, __obj, __field)          \
do {                                                                     \
    if (((__len) = fiid_obj_field_len_bytes ((__obj), (__field))) < 0)   \
      {                                                                  \
         __FIID_OBJ_SYSLOG((__obj));                                     \
         __FIID_OBJ_TRACE((__obj));                                      \
         __FIID_OBJ_SET_ERRNO((__obj));                                  \
         goto cleanup;                                                   \
      }                                                                  \
} while (0)

#define FIID_OBJ_BLOCK_LEN(__len, __obj, __field_start, __field_end)                          \
do {                                                                                          \
    if (((__len) = fiid_obj_block_len ((__obj), (__field_start), (__field_end))) < 0)         \
      {                                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                                          \
         __FIID_OBJ_TRACE((__obj));                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                       \
         return (-1);                                                                         \
      }                                                                                       \
} while (0)

#define FIID_OBJ_BLOCK_LEN_BYTES(__len, __obj, __field_start, __field_end)                    \
do {                                                                                          \
    if (((__len) = fiid_obj_block_len_bytes ((__obj), (__field_start), (__field_end))) < 0)   \
      {                                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                                          \
         __FIID_OBJ_TRACE((__obj));                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                       \
         return (-1);                                                                         \
      }                                                                                       \
} while (0)

#define FIID_OBJ_BLOCK_LEN_BYTES_CLEANUP(__len, __obj, __field_start, __field_end)            \
do {                                                                                          \
    if (((__len) = fiid_obj_block_len_bytes ((__obj), (__field_start), (__field_end))) < 0)   \
      {                                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                                          \
         __FIID_OBJ_TRACE((__obj));                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                       \
         goto cleanup;                                                                        \
      }                                                                                       \
} while (0)

#define FIID_OBJ_CLEAR(__obj)                            \
do {                                                     \
    if (fiid_obj_clear ((__obj)) < 0)                    \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         return (-1);                                    \
      }                                                  \
} while (0)

#define UDM_FIID_OBJ_CLEAR(__obj)                        \
do {                                                     \
    if (fiid_obj_clear ((__obj)) < 0)                    \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));             \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_CLEAR_NO_RETURN(__obj)                  \
do {                                                     \
    if ((__obj))                                         \
      fiid_obj_clear ((__obj));                          \
} while (0)

#define FIID_OBJ_CLEAR_CLEANUP(__obj)                    \
do {                                                     \
    if (fiid_obj_clear ((__obj)) < 0)                    \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         goto cleanup;                                   \
      }                                                  \
} while (0)

#define FIID_OBJ_CLEAR_FIELD(__obj, __field)             \
do {                                                     \
    if (fiid_obj_clear_field ((__obj), (__field)) < 0)   \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_FIELD_LOOKUP(__obj, __field)                       \
do {                                                                \
    int32_t __ret;                                                  \
    if ((__ret = fiid_obj_field_lookup ((__obj), (__field))) < 0)   \
      {                                                             \
         __FIID_OBJ_SYSLOG((__obj));                                \
         __FIID_OBJ_TRACE((__obj));                                 \
         __FIID_OBJ_SET_ERRNO((__obj));                             \
         return (-1);                                               \
      }                                                             \
    if (!__ret)                                                     \
      {                                                             \
	errno = EINVAL;                                             \
	return (-1);                                                \
      }                                                             \
} while (0)

#define FIID_OBJ_SET(__obj, __field, __val)              \
do {                                                     \
    if (fiid_obj_set ((__obj), (__field), (__val)) < 0)  \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_SET_CLEANUP(__obj, __field, __val)      \
do {                                                     \
    if (fiid_obj_set ((__obj), (__field), (__val)) < 0)  \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
        goto cleanup;                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_SET_DATA(__obj, __field, __data, __data_len)               \
do {                                                                        \
    if (fiid_obj_set_data ((__obj), (__field), (__data), (__data_len)) < 0) \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_DATA_CLEANUP(__obj, __field, __data, __data_len)       \
do {                                                                        \
    if (fiid_obj_set_data ((__obj), (__field), (__data), (__data_len)) < 0) \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_DATA_LEN(__len, __obj, __field, __data, __data_len)                \
do {                                                                                    \
    if (((__len) = fiid_obj_set_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_OBJ_SYSLOG((__obj));                                                    \
         __FIID_OBJ_TRACE((__obj));                                                     \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         return (-1);                                                                   \
      }                                                                                 \
} while (0)

#define FIID_OBJ_SET_DATA_LEN_CLEANUP(__len, __obj, __field, __data, __data_len)        \
do {                                                                                    \
    if (((__len) = fiid_obj_set_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_OBJ_SYSLOG((__obj));                                                    \
         __FIID_OBJ_TRACE((__obj));                                                     \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         goto cleanup;                                                                  \
      }                                                                                 \
} while (0)

#define FIID_OBJ_SET_ALL(__obj, __data, __data_len)                         \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_CLEANUP(__obj, __data, __data_len)                 \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define UDM_FIID_OBJ_SET_ALL(__obj, __data, __data_len)                     \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                                \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define UDM_FIID_OBJ_SET_ALL_CLEANUP(__obj, __data, __data_len)             \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                                \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_LEN(__len, __obj, __data, __data_len)              \
do {                                                                        \
    if (((__len) = fiid_obj_set_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_LEN_CLEANUP(__len, __obj, __data, __data_len)      \
do {                                                                        \
    if (((__len) = fiid_obj_set_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_BLOCK(__obj, __field_start, __field_end, __data, __data_len)                    \
do {                                                                                                 \
    if (fiid_obj_set_block ((__obj), (__field_start), (__field_end), (__data), (__data_len)) < 0)    \
      {                                                                                              \
         __FIID_OBJ_SYSLOG((__obj));                                                                 \
         __FIID_OBJ_TRACE((__obj));                                                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                                                              \
         return (-1);                                                                                \
      }                                                                                              \
} while (0)

#define FIID_OBJ_SET_BLOCK_CLEANUP(__obj, __field_start, __field_end, __data, __data_len)            \
do {                                                                                                 \
    if (fiid_obj_set_block ((__obj), (__field_start), (__field_end), (__data), (__data_len)) < 0)    \
      {                                                                                              \
         __FIID_OBJ_SYSLOG((__obj));                                                                 \
         __FIID_OBJ_TRACE((__obj));                                                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                                                              \
         goto cleanup;                                                                               \
      }                                                                                              \
} while (0)

#define FIID_OBJ_SET_BLOCK_LEN(__len, __obj, __field_start, __field_end, __data, __data_len)                  \
do {                                                                                                          \
    if (((__len) = fiid_obj_set_block ((__obj), (__field_start), (__field_end), (__data), (__data_len))) < 0) \
      {                                                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                                                          \
         __FIID_OBJ_TRACE((__obj));                                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                                       \
         return (-1);                                                                                         \
      }                                                                                                       \
} while (0)

#define FIID_OBJ_SET_BLOCK_LEN_CLEANUP(__len, __obj, __field_start, __field_end, __data, __data_len)          \
do {                                                                                                          \
    if (((__len) = fiid_obj_set_block ((__obj), (__field_start), (__field_end), (__data), (__data_len))) < 0) \
      {                                                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                                                          \
         __FIID_OBJ_TRACE((__obj));                                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                                       \
         goto cleanup;                                                                                        \
      }                                                                                                       \
} while (0)

#define FIID_OBJ_GET(__obj, __field, __val)                   \
do {                                                          \
    uint64_t __localval = 0, *__localval_ptr;                 \
    __localval_ptr = (__val);                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)   \
      {                                                       \
         __FIID_OBJ_SYSLOG((__obj));                          \
         __FIID_OBJ_TRACE((__obj));                           \
         __FIID_OBJ_SET_ERRNO((__obj));                       \
         return (-1);                                         \
      }                                                       \
    *__localval_ptr = __localval;                             \
} while (0)

#define FIID_OBJ_GET_CLEANUP(__obj, __field, __val)           \
do {                                                          \
    uint64_t __localval = 0, *__localval_ptr;                 \
    __localval_ptr = (__val);                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)   \
      {                                                       \
         __FIID_OBJ_SYSLOG((__obj));                          \
         __FIID_OBJ_TRACE((__obj));                           \
         __FIID_OBJ_SET_ERRNO((__obj));                       \
         goto cleanup;                                        \
      }                                                       \
    *__localval_ptr = __localval;                             \
} while (0)

#define UDM_FIID_OBJ_GET(__obj, __field, __val)               \
do {                                                          \
    uint64_t __localval = 0, *__localval_ptr;                 \
    __localval_ptr = (__val);                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)   \
      {                                                       \
         __FIID_OBJ_SYSLOG((__obj));                          \
         __FIID_OBJ_TRACE((__obj));                           \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                  \
         return (-1);                                         \
      }                                                       \
    *__localval_ptr = __localval;                             \
} while (0)

#define UDM_FIID_OBJ_GET_CLEANUP(__obj, __field, __val)       \
do {                                                          \
    uint64_t __localval = 0, *__localval_ptr;                 \
    __localval_ptr = (__val);                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)   \
      {                                                       \
         __FIID_OBJ_SYSLOG((__obj));                          \
         __FIID_OBJ_TRACE((__obj));                           \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                  \
         goto cleanup;                                        \
      }                                                       \
    *__localval_ptr = __localval;                             \
} while (0)

#define FIID_OBJ_GET_DATA(__obj, __field, __data, __data_len)                \
do {                                                                         \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)  \
      {                                                                      \
         __FIID_OBJ_SYSLOG((__obj));                                         \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         return (-1);                                                        \
      }                                                                      \
} while (0)

#define FIID_OBJ_GET_DATA_CLEANUP(__obj, __field, __data, __data_len)        \
do {                                                                         \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)  \
      {                                                                      \
         __FIID_OBJ_SYSLOG((__obj));                                         \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         goto cleanup;                                                       \
      }                                                                      \
} while (0)

#define UDM_FIID_OBJ_GET_DATA(__obj, __field, __data, __data_len)            \
do {                                                                         \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)  \
      {                                                                      \
         __FIID_OBJ_SYSLOG((__obj));                                         \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                                 \
         return (-1);                                                        \
      }                                                                      \
} while (0)

#define UDM_FIID_OBJ_GET_DATA_CLEANUP(__obj, __field, __data, __data_len)    \
do {                                                                         \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)  \
      {                                                                      \
         __FIID_OBJ_SYSLOG((__obj));                                         \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                                 \
         goto cleanup;                                                       \
      }                                                                      \
} while (0)

#define FIID_OBJ_GET_DATA_LEN(__len, __obj, __field, __data, __data_len)                \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_OBJ_SYSLOG((__obj));                                                    \
         __FIID_OBJ_TRACE((__obj));                                                     \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         return (-1);                                                                   \
      }                                                                                 \
} while (0)

#define FIID_OBJ_GET_DATA_LEN_CLEANUP(__len, __obj, __field, __data, __data_len)        \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_OBJ_SYSLOG((__obj));                                                    \
         __FIID_OBJ_TRACE((__obj));                                                     \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         goto cleanup;                                                                  \
      }                                                                                 \
} while (0)

#define FIID_OBJ_GET_ALL(__obj, __data, __data_len)                          \
do {                                                                         \
    if (fiid_obj_get_all ((__obj), (__data), (__data_len)) < 0)              \
      {                                                                      \
         __FIID_OBJ_SYSLOG((__obj));                                         \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         return (-1);                                                        \
      }                                                                      \
} while (0)

#define FIID_OBJ_GET_ALL_CLEANUP(__obj, __data, __data_len)                  \
do {                                                                         \
    if (fiid_obj_get_all ((__obj), (__data), (__data_len)) < 0)              \
      {                                                                      \
         __FIID_OBJ_SYSLOG((__obj));                                         \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         goto cleanup;                                                       \
      }                                                                      \
} while (0)

#define UDM_FIID_OBJ_GET_ALL(__obj, __data, __data_len)                      \
do {                                                                         \
    if (fiid_obj_get_all ((__obj), (__data), (__data_len)) < 0)              \
      {                                                                      \
         __FIID_OBJ_SYSLOG((__obj));                                         \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                                 \
         return (-1);                                                        \
      }                                                                      \
} while (0)

#define UDM_FIID_OBJ_GET_ALL_CLEANUP(__obj, __data, __data_len)              \
do {                                                                         \
    if (fiid_obj_get_all ((__obj), (__data), (__data_len)) < 0)              \
      {                                                                      \
         __FIID_OBJ_SYSLOG((__obj));                                         \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                                 \
         goto cleanup;                                                       \
      }                                                                      \
} while (0)
 
#define FIID_OBJ_GET_ALL_LEN(__len, __obj, __data, __data_len)              \
do {                                                                        \
    if (((__len) = fiid_obj_get_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_GET_ALL_LEN_CLEANUP(__len, __obj, __data, __data_len)      \
do {                                                                        \
    if (((__len) = fiid_obj_get_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define UDM_FIID_OBJ_GET_ALL_LEN(__len, __obj, __data, __data_len)          \
do {                                                                        \
    if (((__len) = fiid_obj_get_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                                \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define UDM_FIID_OBJ_GET_ALL_LEN_CLEANUP(__len, __obj, __data, __data_len)  \
do {                                                                        \
    if (((__len) = fiid_obj_get_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_SYSLOG((__obj));                                        \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                                \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_GET_BLOCK(__obj, __field_start, __field_end, __data, __data_len)                    \
do {                                                                                                 \
    if (fiid_obj_get_block ((__obj), (__field_start), (__field_end), (__data), (__data_len)) < 0)    \
      {                                                                                              \
         __FIID_OBJ_SYSLOG((__obj));                                                                 \
         __FIID_OBJ_TRACE((__obj));                                                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                                                              \
         return (-1);                                                                                \
      }                                                                                              \
} while (0)

#define FIID_OBJ_GET_BLOCK_CLEANUP(__obj, __field_start, __field_end, __data, __data_len)            \
do {                                                                                                 \
    if (fiid_obj_get_block ((__obj), (__field_start), (__field_end), (__data), (__data_len)) < 0)    \
      {                                                                                              \
         __FIID_OBJ_SYSLOG((__obj));                                                                 \
         __FIID_OBJ_TRACE((__obj));                                                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                                                              \
         goto cleanup;                                                                               \
      }                                                                                              \
} while (0)

#define FIID_OBJ_GET_BLOCK_LEN(__len, __obj, __field_start, __field_end, __data, __data_len)                  \
do {                                                                                                          \
    if (((__len) = fiid_obj_get_block ((__obj), (__field_start), (__field_end), (__data), (__data_len))) < 0) \
      {                                                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                                                          \
         __FIID_OBJ_TRACE((__obj));                                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                                       \
         return (-1);                                                                                         \
      }                                                                                                       \
} while (0)

#define FIID_OBJ_GET_BLOCK_LEN_CLEANUP(__len, __obj, __field_start, __field_end, __data, __data_len)          \
do {                                                                                                          \
    if (((__len) = fiid_obj_get_block ((__obj), (__field_start), (__field_end), (__data), (__data_len))) < 0) \
      {                                                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                                                          \
         __FIID_OBJ_TRACE((__obj));                                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                                       \
         goto cleanup;                                                                                        \
      }                                                                                                       \
} while (0)

#define FIID_OBJ_PACKET_VALID(__obj)                   \
do {                                                   \
    int __ret;                                         \
    if ((__ret = fiid_obj_packet_valid((__obj))) < 0)  \
      {                                                \
        __FIID_OBJ_SYSLOG((__obj));                    \
        __FIID_OBJ_TRACE((__obj));                     \
	__FIID_OBJ_SET_ERRNO((__obj));                 \
        return (-1);                                   \
      }                                                \
    if (!__ret)                                        \
      {                                                \
	errno = EINVAL;                                \
        __FIID_OBJ_SYSLOG((__obj));                    \
        __FIID_OBJ_TRACE((__obj));                     \
	return (-1);                                   \
      }                                                \
} while (0)

#define UDM_FIID_OBJ_PACKET_VALID(__obj)               \
do {                                                   \
    int __ret;                                         \
    if ((__ret = fiid_obj_packet_valid((__obj))) < 0)  \
      {                                                \
        __FIID_OBJ_SYSLOG((__obj));                    \
        __FIID_OBJ_TRACE((__obj));                     \
	__FIID_OBJ_SET_UDM_ERRNUM((__obj));            \
        return (-1);                                   \
      }                                                \
    if (!__ret)                                        \
      {                                                \
        __FIID_OBJ_SYSLOG((__obj));                    \
        __FIID_OBJ_TRACE((__obj));                     \
        dev->errnum = IPMI_ERR_INVALID_PARAMETERS;     \
	return (-1);                                   \
      }                                                \
} while (0)

#define FIID_OBJ_TEMPLATE(__ptr, __obj)                \
do {                                                   \
    if (!(__ptr = fiid_obj_template((__obj))))         \
      {                                                \
        __FIID_OBJ_SYSLOG((__obj));                    \
        __FIID_OBJ_TRACE((__obj));                     \
	__FIID_OBJ_SET_ERRNO((__obj));                 \
        return (-1);                                   \
      }                                                \
} while (0)

#define FIID_OBJ_TEMPLATE_CLEANUP(__ptr, __obj)        \
do {                                                   \
    if (!(__ptr = fiid_obj_template((__obj))))         \
      {                                                \
        __FIID_OBJ_SYSLOG((__obj));                    \
        __FIID_OBJ_TRACE((__obj));                     \
	__FIID_OBJ_SET_ERRNO((__obj));                 \
        goto cleanup;                                  \
      }                                                \
} while (0)

#define UDM_FIID_OBJ_TEMPLATE_CLEANUP(__ptr, __obj)    \
do {                                                   \
    if (!(__ptr = fiid_obj_template((__obj))))         \
      {                                                \
        __FIID_OBJ_SYSLOG((__obj));                    \
        __FIID_OBJ_TRACE((__obj));                     \
	__FIID_OBJ_SET_UDM_ERRNUM((__obj));            \
        goto cleanup;                                  \
      }                                                \
} while (0)

#define FIID_OBJ_TEMPLATE_COMPARE(__obj, __tmpl)                     \
do {                                                                 \
    int __ret;                                                       \
    if ((__ret = fiid_obj_template_compare ((__obj), (__tmpl))) < 0) \
      {                                                              \
         __FIID_OBJ_SYSLOG((__obj));                                 \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                              \
         return (-1);                                                \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
         __FIID_OBJ_SYSLOG((__obj));                                 \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                              \
	return (-1);                                                 \
      }                                                              \
} while (0)

#define FIID_OBJ_TEMPLATE_COMPARE_CLEANUP(__obj, __tmpl)             \
do {                                                                 \
    int __ret;                                                       \
    if ((__ret = fiid_obj_template_compare ((__obj), (__tmpl))) < 0) \
      {                                                              \
         __FIID_OBJ_SYSLOG((__obj));                                 \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                              \
         goto cleanup;                                               \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
         __FIID_OBJ_SYSLOG((__obj));                                 \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                              \
	goto cleanup;                                                \
      }                                                              \
} while (0)

#define UDM_FIID_OBJ_TEMPLATE_COMPARE(__obj, __tmpl)                 \
do {                                                                 \
    int __ret;                                                       \
    if ((__ret = fiid_obj_template_compare ((__obj), (__tmpl))) < 0) \
      {                                                              \
         __FIID_OBJ_SYSLOG((__obj));                                 \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                         \
         return (-1);                                                \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
         __FIID_OBJ_SYSLOG((__obj));                                 \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                         \
	return (-1);                                                 \
      }                                                              \
} while (0)

#define UDM_FIID_OBJ_TEMPLATE_COMPARE_CLEANUP(__obj, __tmpl)         \
do {                                                                 \
    int __ret;                                                       \
    if ((__ret = fiid_obj_template_compare ((__obj), (__tmpl))) < 0) \
      {                                                              \
         __FIID_OBJ_SYSLOG((__obj));                                 \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                         \
         goto cleanup;                                               \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
         __FIID_OBJ_SYSLOG((__obj));                                 \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_UDM_ERRNUM((__obj));                         \
	goto cleanup;                                                \
      }                                                              \
} while (0)

#define FIID_ITERATOR_CREATE(__iter, __obj)                          \
do {                                                                 \
  if (!((__iter) = fiid_iterator_create((__obj))))                   \
    {                                                                \
      __FIID_OBJ_SYSLOG((__obj));                                    \
      __FIID_OBJ_TRACE((__obj));                                     \
      __FIID_OBJ_SET_ERRNO((__obj));                                 \
      return (-1);                                                   \
    }                                                                \
} while (0)

#define FIID_ITERATOR_KEY(__key, __iter)                             \
do {                                                                 \
  if (!((__key) = fiid_iterator_key((__iter))))                      \
    {                                                                \
      __FIID_ITER_SYSLOG((__iter));                                  \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      return (-1);                                                   \
    }                                                                \
} while (0)

#define FIID_ITERATOR_KEY_CLEANUP(__key, __iter)                     \
do {                                                                 \
  if (!((__key) = fiid_iterator_key((__iter))))                      \
    {                                                                \
      __FIID_ITER_SYSLOG((__iter));                                  \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      goto cleanup;                                                  \
    }                                                                \
} while (0)

#define FIID_ITERATOR_FIELD_LEN(__field_len, __iter)                 \
do {                                                                 \
  if (((__field_len) = fiid_iterator_field_len((__iter))) < 0)       \
    {                                                                \
      __FIID_ITER_SYSLOG((__iter));                                  \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      return (-1);                                                   \
    }                                                                \
} while (0)

#define FIID_ITERATOR_FIELD_LEN_CLEANUP(__field_len, __iter)         \
do {                                                                 \
  if (((__field_len) = fiid_iterator_field_len((__iter))) < 0)       \
    {                                                                \
      __FIID_ITER_SYSLOG((__iter));                                  \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      goto cleanup;                                                  \
    }                                                                \
} while (0)

#define FIID_ITERATOR_GET(__iter, __val)                             \
do {                                                                 \
  if (fiid_iterator_get((__iter), (__val)) < 0)                      \
    {                                                                \
      __FIID_ITER_SYSLOG((__iter));                                  \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      return (-1);                                                   \
    }                                                                \
} while (0)

#define FIID_ITERATOR_GET_CLEANUP(__iter, __val)                     \
do {                                                                 \
  if (fiid_iterator_get((__iter), (__val)) < 0)                      \
    {                                                                \
      __FIID_ITER_SYSLOG((__iter));                                  \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      goto cleanup;                                                  \
    }                                                                \
} while (0)

#define FIID_ITERATOR_GET_DATA(__iter, __data, __data_len)           \
do {                                                                 \
  if (fiid_iterator_get_data((__iter), (__data), (__data_len)) < 0)  \
    {                                                                \
      __FIID_ITER_SYSLOG((__iter));                                  \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      return (-1);                                                   \
    }                                                                \
} while (0)

#define FIID_ITERATOR_GET_DATA_CLEANUP(__iter, __data, __data_len)   \
do {                                                                 \
  if (fiid_iterator_get_data((__iter), (__data), (__data_len)) < 0)  \
    {                                                                \
      __FIID_ITER_SYSLOG((__iter));                                  \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      goto cleanup;                                                  \
    }                                                                \
} while (0)

#define FIID_ITERATOR_GET_DATA_LEN(__len, __iter, __data, __data_len)            \
do {                                                                             \
  if (((__len) = fiid_iterator_get_data((__iter), (__data), (__data_len))) < 0)  \
    {                                                                            \
      __FIID_ITER_SYSLOG((__iter));                                              \
      __FIID_ITER_TRACE((__iter));                                               \
      __FIID_ITER_SET_ERRNO((__iter));                                           \
      return (-1);                                                               \
    }                                                                            \
} while (0)

#define FIID_ITERATOR_GET_DATA_LEN_CLEANUP(__len, __iter, __data, __data_len)    \
do {                                                                             \
  if (((__len) = fiid_iterator_get_data((__iter), (__data), (__data_len))) < 0)  \
    {                                                                            \
      __FIID_ITER_SYSLOG((__iter));                                              \
      __FIID_ITER_TRACE((__iter));                                               \
      __FIID_ITER_SET_ERRNO((__iter));                                           \
      goto cleanup;                                                              \
    }                                                                            \
} while (0)


#ifdef __cplusplus
}
#endif

#endif /* fiid-wrappers.h */
