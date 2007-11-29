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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

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
#define __FIID_ERRNUM_SYSLOG(___errnum)                              \
do {                                                                 \
  char __errbuf[ERR_WRAPPER_STR_MAX_LEN];                            \
  snprintf (__errbuf, FIID_WRAPPER_STR_MAX_LEN,                      \
	    "%s: %d: %s: error = %s (%d)",                           \
            __FILE__, __LINE__, __PRETTY_FUNCTION__,                 \
	    fiid_strerror(___errnum),                                \
            ___errnum);                                              \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), __errbuf);    \
} while (0)

#define __FIID_OBJ_SYSLOG(___obj)                                    \
do {                                                                 \
  int32_t __obj_errnum = fiid_obj_errnum((___obj));                  \
  __FIID_ERRNUM_SYSLOG(__obj_errnum);                                \
} while (0)

#define __FIID_ITER_SYSLOG(___iter)                                  \
do {                                                                 \
  int32_t __iter_errnum = fiid_iterator_errnum((___iter));           \
  __FIID_ERRNUM_SYSLOG(__iter_errnum);                               \
} while (0)
#else
#define __FIID_ERRNUM_SYSLOG(___errnum)
#define __FIID_OBJ_SYSLOG(___obj)
#define __FIID_ITER_SYSLOG(___iter)
#endif /* IPMI_SYSLOG */

#if defined (IPMI_TRACE)
#define __FIID_ERRNUM_TRACE(___errnum)                             \
do {                                                               \
  fprintf (stderr,                                                 \
	   "%s: %d: %s: error = %s (%d)",                          \
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

#define __FIID_ITER_TRACE(___iter)                                 \
do {                                                               \
  int32_t __iter_errnum = fiid_iterator_errnum((___iter));         \
  __FIID_ERRNUM_TRACE(__iter_errnum);                              \
} while (0)

#else
#define __FIID_ERRNUM_TRACE(___errnum)
#define __FIID_OBJ_TRACE(___obj)
#define __FIID_ITER_TRACE(___iter)
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

#define __FIID_ITER_SET_ERRNO(___iter)                      \
do {                                                        \
  int32_t ___itererrnum = fiid_iterator_errnum((___iter));  \
  __FIID_ERRNUM_SET_ERRNO(___itererrnum);                   \
} while (0)

#define FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                           \
do {                                                                     \
  fiid_err_t __err;                                                      \
  if (((__len) = fiid_template_len_bytes (&__err, (__tmpl))) < 0)        \
    {                                                                    \
      __FIID_ERRNUM_SYSLOG(__err);                                       \
      __FIID_ERRNUM_TRACE(__err);                                        \
      __FIID_ERRNUM_SET_ERRNO(__err);                                    \
      return (-1);                                                       \
    }                                                                    \
} while (0)

#define FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)                   \
do {                                                                     \
  fiid_err_t __err;                                                      \
  if (((__len) = fiid_template_len_bytes (&__err, (__tmpl))) < 0)        \
    {                                                                    \
      __FIID_ERRNUM_SYSLOG(__err);                                       \
      __FIID_ERRNUM_TRACE(__err);                                        \
      __FIID_ERRNUM_SET_ERRNO(__err);                                    \
      goto cleanup;                                                      \
    }                                                                    \
} while (0)

#define FIID_TEMPLATE_FIELD_START_BYTES(__len, __tmpl, __field)                      \
do {                                                                                 \
  fiid_err_t __err;                                                                  \
  if (((__len) = fiid_template_field_start_bytes (&__err, (__tmpl), (__field))) < 0) \
    {                                                                                \
      __FIID_ERRNUM_SYSLOG(__err);                                                   \
      __FIID_ERRNUM_TRACE(__err);                                                    \
      __FIID_ERRNUM_SET_ERRNO(__err);                                                \
      return (-1);                                                                   \
    }                                                                                \
} while (0)

#define FIID_TEMPLATE_FIELD_START_BYTES_CLEANUP(__len, __tmpl, __field)              \
do {                                                                                 \
  fiid_err_t __err;                                                                  \
  if (((__len) = fiid_template_field_start_bytes (&__err, (__tmpl), (__field))) < 0) \
    {                                                                                \
      __FIID_ERRNUM_SYSLOG(__err);                                                   \
      __FIID_ERRNUM_TRACE(__err);                                                    \
      __FIID_ERRNUM_SET_ERRNO(__err);                                                \
      goto cleanup;                                                                  \
    }                                                                                \
} while (0)

#define FIID_TEMPLATE_FIELD_LEN(__len, __tmpl, __field)                            \
do {                                                                               \
  fiid_err_t __err;                                                                \
  if (((__len) = fiid_template_field_len (&__err, (__tmpl), (__field))) < 0)       \
    {                                                                              \
      __FIID_ERRNUM_SYSLOG(__err);                                                 \
      __FIID_ERRNUM_TRACE(__err);                                                  \
      __FIID_ERRNUM_SET_ERRNO(__err);                                              \
      return (-1);                                                                 \
    }                                                                              \
} while (0)

#define FIID_TEMPLATE_FIELD_LEN_BYTES(__len, __tmpl, __field)                      \
do {                                                                               \
  fiid_err_t __err;                                                                \
  if (((__len) = fiid_template_field_len_bytes (&__err, (__tmpl), (__field))) < 0) \
    {                                                                              \
      __FIID_ERRNUM_SYSLOG(__err);                                                 \
      __FIID_ERRNUM_TRACE(__err);                                                  \
      __FIID_ERRNUM_SET_ERRNO(__err);                                              \
      return (-1);                                                                 \
    }                                                                              \
} while (0)

#define FIID_TEMPLATE_FIELD_LEN_BYTES_CLEANUP(__len, __tmpl, __field)              \
do {                                                                               \
  fiid_err_t __err;                                                                \
  if (((__len) = fiid_template_field_len_bytes (&__err, (__tmpl), (__field))) < 0) \
    {                                                                              \
      __FIID_ERRNUM_SYSLOG(__err);                                                 \
      __FIID_ERRNUM_TRACE(__err);                                                  \
      __FIID_ERRNUM_SET_ERRNO(__err);                                              \
      goto cleanup;                                                                \
    }                                                                              \
} while (0)

#define FIID_TEMPLATE_BLOCK_LEN_BYTES(__len, __tmpl, __field_start, __field_end)                        \
do {                                                                                                    \
  fiid_err_t __err;                                                                                     \
  if (((__len) = fiid_template_block_len_bytes (&__err, (__tmpl), (__field_start), (__field_end))) < 0) \
    {                                                                                                   \
      __FIID_ERRNUM_SYSLOG(__err);                                                                      \
      __FIID_ERRNUM_TRACE(__err);                                                                       \
      __FIID_ERRNUM_SET_ERRNO(__err);                                                                   \
      return (-1);                                                                                      \
    }                                                                                                   \
} while (0)

#define FIID_TEMPLATE_COMPARE(__tmpl1, __tmpl2)                              \
do {                                                                         \
    int __ret;                                                               \
    fiid_err_t __err;                                                        \
    if ((__ret = fiid_template_compare (&__err, (__tmpl1), (__tmpl2))) < 0)  \
      {                                                                      \
        __FIID_ERRNUM_SYSLOG(__err);                                         \
        __FIID_ERRNUM_TRACE(__err);                                          \
        __FIID_ERRNUM_SET_ERRNO(__err);                                      \
        return (-1);                                                         \
      }                                                                      \
    if (!__ret)                                                              \
      {                                                                      \
	errno = EINVAL;                                                      \
        __FIID_ERRNUM_SYSLOG(__err);                                         \
        __FIID_ERRNUM_TRACE(__err);                                          \
	return (-1);                                                         \
      }                                                                      \
} while (0)

#define FIID_TEMPLATE_COMPARE_CLEANUP(__tmpl1, __tmpl2)                      \
do {                                                                         \
    int __ret;                                                               \
    fiid_err_t __err;                                                        \
    if ((__ret = fiid_template_compare (&__err, (__tmpl1), (__tmpl2))) < 0)  \
      {                                                                      \
        __FIID_ERRNUM_SYSLOG(__err);                                         \
        __FIID_ERRNUM_TRACE(__err);                                          \
        __FIID_ERRNUM_SET_ERRNO(__err);                                      \
        goto cleanup;                                                        \
      }                                                                      \
    if (!__ret)                                                              \
      {                                                                      \
	errno = EINVAL;                                                      \
        __FIID_ERRNUM_SYSLOG(__err);                                         \
        __FIID_ERRNUM_TRACE(__err);                                          \
	goto cleanup;                                                        \
      }                                                                      \
} while (0)

#define FIID_TEMPLATE_FREE(__tmpl)             \
do {                                           \
  if ((__tmpl))                                \
    fiid_template_free((__tmpl));              \
} while (0)

#define FIID_OBJ_CREATE(__obj, __tmpl)                \
do {                                                  \
  fiid_err_t __err;                                   \
  if (!((__obj) = fiid_obj_create(&__err, __tmpl)))   \
    {                                                 \
      __FIID_ERRNUM_SYSLOG(__err);                    \
      __FIID_ERRNUM_TRACE(__err);                     \
      __FIID_ERRNUM_SET_ERRNO(__err);                 \
      return (-1);                                    \
    }                                                 \
} while (0)

#define FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)        \
do {                                                  \
  fiid_err_t __err;                                   \
  if (!((__obj) = fiid_obj_create(&__err, __tmpl)))   \
    {                                                 \
      __FIID_ERRNUM_SYSLOG(__err);                    \
      __FIID_ERRNUM_TRACE(__err);                     \
      __FIID_ERRNUM_SET_ERRNO(__err);                 \
      goto cleanup;                                   \
    }                                                 \
} while (0)

#define FIID_OBJ_DESTROY(__obj)                \
do {                                           \
  if ((__obj))                                 \
    {                                          \
      fiid_obj_destroy((__obj));               \
      (__obj) = NULL;                          \
    }                                          \
} while (0)

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

#define __FIID_ERRNUM_TO_KCS_ERRNUM(___errnum)                                \
do {                                                                          \
  if ((___errnum) == 0)                                                       \
    ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;                                   \
  else if ((___errnum) == ENOMEM)                                             \
    ctx->errnum = IPMI_KCS_CTX_ERR_OUT_OF_MEMORY;                             \
  else                                                                        \
    ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL_ERROR;                            \
} while (0)

#define __FIID_OBJ_ERRNUM_TO_KCS_ERRNUM(___obj)                               \
do {                                                                          \
  int32_t __obj_errnum = fiid_obj_errnum((___obj));                           \
  __FIID_ERRNUM_TO_KCS_ERRNUM(__obj_errnum);                                  \
} while (0)

#define KCS_FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                            \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (((__len) = fiid_template_len_bytes (&__err, (__tmpl))) < 0)             \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      __FIID_ERRNUM_TO_KCS_ERRNUM(__err);                                     \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define KCS_FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)                    \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (((__len) = fiid_template_len_bytes (&__err, (__tmpl))) < 0)             \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      __FIID_ERRNUM_TO_KCS_ERRNUM(__err);                                     \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define KCS_FIID_TEMPLATE_FREE(__tmpl) FIID_TEMPLATE_FREE((__tmpl))

#define KCS_FIID_OBJ_CREATE(__obj, __tmpl)                                   \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (!((__obj) = fiid_obj_create(&__err, __tmpl)))                           \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      __FIID_ERRNUM_TO_KCS_ERRNUM(__err);                                     \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define KCS_FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)                            \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (!((__obj) = fiid_obj_create(&__err, __tmpl)))                           \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      __FIID_ERRNUM_TO_KCS_ERRNUM(__err);                                     \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define KCS_FIID_OBJ_DESTROY(__obj) FIID_OBJ_DESTROY((__obj))

#define KCS_FIID_OBJ_LEN_BYTES(__len, __obj)             \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_ERRNUM_TO_KCS_ERRNUM((__obj));       \
         return (-1);                                    \
      }                                                  \
} while (0)

#define KCS_FIID_OBJ_TEMPLATE(__ptr, __obj)              \
do {                                                     \
    if (!(__ptr = fiid_obj_template((__obj))))           \
      {                                                  \
        __FIID_OBJ_SYSLOG((__obj));                      \
        __FIID_OBJ_TRACE((__obj));                       \
        __FIID_OBJ_ERRNUM_TO_KCS_ERRNUM((__obj));        \
        return (-1);                                     \
      }                                                  \
} while (0)

#define KCS_FIID_OBJ_TEMPLATE_CLEANUP(__ptr, __obj)      \
do {                                                     \
    if (!(__ptr = fiid_obj_template((__obj))))           \
      {                                                  \
        __FIID_OBJ_SYSLOG((__obj));                      \
        __FIID_OBJ_TRACE((__obj));                       \
        __FIID_OBJ_ERRNUM_TO_KCS_ERRNUM((__obj));        \
        goto cleanup;                                    \
      }                                                  \
} while (0)

#define __FIID_ERRNUM_TO_SSIF_ERRNUM(___errnum)                               \
do {                                                                          \
  if ((___errnum) == 0)                                                       \
    ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;                                  \
  else if ((___errnum) == ENOMEM)                                             \
    ctx->errnum = IPMI_SSIF_CTX_ERR_OUT_OF_MEMORY;                            \
  else                                                                        \
    ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL_ERROR;                           \
} while (0)

#define __FIID_OBJ_ERRNUM_TO_SSIF_ERRNUM(___obj)                              \
do {                                                                          \
  int32_t __obj_errnum = fiid_obj_errnum((___obj));                           \
  __FIID_ERRNUM_TO_SSIF_ERRNUM(__obj_errnum);                                 \
} while (0)

#define SSIF_FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                           \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (((__len) = fiid_template_len_bytes (&__err, (__tmpl))) < 0)             \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      __FIID_ERRNUM_TO_SSIF_ERRNUM(__err);                                    \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define SSIF_FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)                   \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (((__len) = fiid_template_len_bytes (&__err, (__tmpl))) < 0)             \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      __FIID_ERRNUM_TO_SSIF_ERRNUM(__err);                                    \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define SSIF_FIID_TEMPLATE_FREE(__tmpl) FIID_TEMPLATE_FREE((__tmpl))

#define SSIF_FIID_OBJ_CREATE(__obj, __tmpl)                                   \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (!((__obj) = fiid_obj_create(&__err, __tmpl)))                           \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      __FIID_ERRNUM_TO_SSIF_ERRNUM(__err);                                    \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define SSIF_FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)                           \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (!((__obj) = fiid_obj_create(&__err, __tmpl)))                           \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      __FIID_ERRNUM_TO_SSIF_ERRNUM(__err);                                    \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define SSIF_FIID_OBJ_DESTROY(__obj) FIID_OBJ_DESTROY((__obj))

#define SSIF_FIID_OBJ_LEN_BYTES(__len, __obj)            \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_SYSLOG((__obj));                     \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_ERRNUM_TO_SSIF_ERRNUM((__obj));      \
         return (-1);                                    \
      }                                                  \
} while (0)

#define SSIF_FIID_OBJ_TEMPLATE(__ptr, __obj)             \
do {                                                     \
    if (!(__ptr = fiid_obj_template((__obj))))           \
      {                                                  \
        __FIID_OBJ_SYSLOG((__obj));                      \
        __FIID_OBJ_TRACE((__obj));                       \
        __FIID_OBJ_ERRNUM_TO_SSIF_ERRNUM((__obj));       \
        return (-1);                                     \
      }                                                  \
} while (0)

#define SSIF_FIID_OBJ_TEMPLATE_CLEANUP(__ptr, __obj)     \
do {                                                     \
    if (!(__ptr = fiid_obj_template((__obj))))           \
      {                                                  \
        __FIID_OBJ_SYSLOG((__obj));                      \
        __FIID_OBJ_TRACE((__obj));                       \
        __FIID_OBJ_ERRNUM_TO_SSIF_ERRNUM((__obj));       \
        goto cleanup;                                    \
      }                                                  \
} while (0)

#define __FIID_ERRNUM_TO_LOCATE_ERRNUM(___errnum)                             \
do {                                                                          \
  if ((___errnum) == 0)                                                       \
    (*locate_errnum) = IPMI_LOCATE_ERR_SUCCESS;                               \
  else if ((___errnum) == ENOMEM)                                             \
    (*locate_errnum) = IPMI_LOCATE_ERR_OUT_OF_MEMORY;                         \
  else                                                                        \
    (*locate_errnum) = IPMI_LOCATE_ERR_INTERNAL_ERROR;                        \
} while (0)

#define __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM(___obj)                            \
do {                                                                          \
  int32_t __obj_errnum = fiid_obj_errnum((___obj));                           \
  __FIID_ERRNUM_TO_LOCATE_ERRNUM(__obj_errnum);                               \
} while (0)

#define LOCATE_FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                         \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (((__len) = fiid_template_len_bytes (&__err, (__tmpl))) < 0)             \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      __FIID_ERRNUM_TO_LOCATE_ERRNUM(__err);                                  \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define LOCATE_FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)                 \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (((__len) = fiid_template_len_bytes (&__err, (__tmpl))) < 0)             \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      __FIID_ERRNUM_TO_LOCATE_ERRNUM(__err);                                  \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define LOCATE_FIID_TEMPLATE_FIELD_LEN_BYTES(__len, __tmpl, __field)                  \
do {                                                                                  \
  fiid_err_t __err;                                                                   \
  if (((__len) = fiid_template_field_len_bytes (&__err, (__tmpl), (__field))) < 0)    \
    {                                                                                 \
      __FIID_ERRNUM_SYSLOG(__err);                                                    \
      __FIID_ERRNUM_TRACE(__err);                                                     \
      __FIID_ERRNUM_TO_LOCATE_ERRNUM(__err);                                          \
      return (-1);                                                                    \
    }                                                                                 \
} while (0)

#define LOCATE_FIID_TEMPLATE_FIELD_LEN_BYTES_CLEANUP(__len, __tmpl, __field)          \
do {                                                                                  \
  fiid_err_t __err;                                                                   \
  if (((__len) = fiid_template_field_len_bytes (&__err, (__tmpl), (__field))) < 0)    \
    {                                                                                 \
      __FIID_ERRNUM_SYSLOG(__err);                                                    \
      __FIID_ERRNUM_TRACE(__err);                                                     \
      __FIID_ERRNUM_TO_LOCATE_ERRNUM(__err);                                          \
      goto cleanup;                                                                   \
    }                                                                                 \
} while (0)

#define LOCATE_FIID_OBJ_CREATE(__obj, __tmpl)                                 \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (!((__obj) = fiid_obj_create(&__err, __tmpl)))                           \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      (*locate_errnum) = IPMI_LOCATE_ERR_OUT_OF_MEMORY;                       \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define LOCATE_FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)                         \
do {                                                                          \
  fiid_err_t __err;                                                           \
  if (!((__obj) = fiid_obj_create(&__err, __tmpl)))                           \
    {                                                                         \
      __FIID_ERRNUM_SYSLOG(__err);                                            \
      __FIID_ERRNUM_TRACE(__err);                                             \
      (*locate_errnum) = IPMI_LOCATE_ERR_OUT_OF_MEMORY;                       \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define LOCATE_FIID_OBJ_DESTROY(__obj) FIID_OBJ_DESTROY((__obj))

#define LOCATE_FIID_OBJ_TEMPLATE_COMPARE(__obj, __tmpl)                       \
do {                                                                          \
    int __ret;                                                                \
    if ((__ret = fiid_obj_template_compare ((__obj), (__tmpl))) < 0)          \
      {                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                          \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         return (-1);                                                         \
      }                                                                       \
    if (!__ret)                                                               \
      {                                                                       \
        __FIID_OBJ_SYSLOG((__obj));                                           \
        __FIID_OBJ_TRACE((__obj));                                            \
        (*locate_errnum) = IPMI_LOCATE_ERR_INTERNAL_ERROR;                    \
	return (-1);                                                          \
      }                                                                       \
} while (0)

#define LOCATE_FIID_OBJ_SET_ALL(__obj, __data, __data_len)                    \
do {                                                                          \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)               \
      {                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                          \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         return (-1);                                                         \
      }                                                                       \
} while (0)

#define LOCATE_FIID_OBJ_SET_ALL_CLEANUP(__obj, __data, __data_len)            \
do {                                                                          \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)               \
      {                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                          \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         goto cleanup;                                                        \
      }                                                                       \
} while (0)

#define LOCATE_FIID_OBJ_GET(__obj, __field, __val)                            \
do {                                                                          \
    uint64_t __localval = 0, *__localval_ptr;                                 \
    __localval_ptr = (__val);                                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)                   \
      {                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                          \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         return (-1);                                                         \
      }                                                                       \
    *__localval_ptr = __localval;                                             \
} while (0)

#define LOCATE_FIID_OBJ_GET_CLEANUP(__obj, __field, __val)                    \
do {                                                                          \
    uint64_t __localval = 0, *__localval_ptr;                                 \
    __localval_ptr = (__val);                                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)                   \
      {                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                          \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         goto cleanup;                                                        \
      }                                                                       \
    *__localval_ptr = __localval;                                             \
} while (0)

#define LOCATE_FIID_OBJ_GET_DATA(__obj, __field, __data, __data_len)          \
do {                                                                          \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)   \
      {                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                          \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         return (-1);                                                         \
      }                                                                       \
} while (0)

#define LOCATE_FIID_OBJ_GET_DATA_CLEANUP(__obj, __field, __data, __data_len)  \
do {                                                                          \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)   \
      {                                                                       \
         __FIID_OBJ_SYSLOG((__obj));                                          \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         goto cleanup;                                                        \
      }                                                                       \
} while (0)

#ifdef __cplusplus
}
#endif

#endif /* fiid-wrappers.h */
