/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
           __FILE__, __LINE__, __FUNCTION__,                         \
           __errnostr, __save_errno);                                \
  fflush (stderr);                                                   \
  errno = __save_errno;                                              \
} while (0)

#define __FIID_ERRNUM_TRACE(___errnum)                             \
do {                                                               \
  fprintf (stderr,                                                 \
	   "%s: %d: %s: error = %s (%d)\n",                        \
           __FILE__, __LINE__, __FUNCTION__,                       \
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

#define __FIID_OBJ_NO_DATA_TRACE(__field)                          \
do {                                                               \
  fprintf (stderr,                                                 \
	   "%s: %d: %s: %s: no data\n",                            \
           __FILE__, __LINE__, __FUNCTION__,                       \
           __field);                                               \
  fflush (stderr);                                                 \
} while (0)

#else
#define __FIID_TRACE
#define __FIID_OBJ_TRACE(___obj)
#define __FIID_ITER_TRACE(___iter)
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

#define __FIID_ITER_SET_ERRNO(___iter)                      \
do {                                                        \
  int32_t ___itererrnum = fiid_iterator_errnum((___iter));  \
  __FIID_ERRNUM_SET_ERRNO(___itererrnum);                   \
} while (0)

#define FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                           \
do {                                                                     \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                \
    {                                                                    \
      __FIID_TRACE;                                                      \
      return (-1);                                                       \
    }                                                                    \
} while (0)

#define FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)                   \
do {                                                                     \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                \
    {                                                                    \
      __FIID_TRACE;                                                      \
      goto cleanup;                                                      \
    }                                                                    \
} while (0)

#define FIID_TEMPLATE_FIELD_START_BYTES(__len, __tmpl, __field)                      \
do {                                                                                 \
  if (((__len) = fiid_template_field_start_bytes ((__tmpl), (__field))) < 0)         \
    {                                                                                \
      __FIID_TRACE;                                                                  \
      return (-1);                                                                   \
    }                                                                                \
} while (0)

#define FIID_TEMPLATE_FIELD_START_BYTES_CLEANUP(__len, __tmpl, __field)              \
do {                                                                                 \
  if (((__len) = fiid_template_field_start_bytes ((__tmpl), (__field))) < 0)         \
    {                                                                                \
      __FIID_TRACE;                                                                  \
      goto cleanup;                                                                  \
    }                                                                                \
} while (0)

#define FIID_TEMPLATE_FIELD_LEN(__len, __tmpl, __field)                            \
do {                                                                               \
  if (((__len) = fiid_template_field_len ((__tmpl), (__field))) < 0)               \
    {                                                                              \
      __FIID_TRACE;                                                                \
      return (-1);                                                                 \
    }                                                                              \
} while (0)

#define FIID_TEMPLATE_FIELD_LEN_BYTES(__len, __tmpl, __field)                      \
do {                                                                               \
  if (((__len) = fiid_template_field_len_bytes ((__tmpl), (__field))) < 0)         \
    {                                                                              \
      __FIID_TRACE;                                                                \
      return (-1);                                                                 \
    }                                                                              \
} while (0)

#define FIID_TEMPLATE_FIELD_LEN_BYTES_CLEANUP(__len, __tmpl, __field)              \
do {                                                                               \
  if (((__len) = fiid_template_field_len_bytes ((__tmpl), (__field))) < 0)         \
    {                                                                              \
      __FIID_TRACE;                                                                \
      goto cleanup;                                                                \
    }                                                                              \
} while (0)

#define FIID_TEMPLATE_BLOCK_LEN_BYTES(__len, __tmpl, __field_start, __field_end)                        \
do {                                                                                                    \
  if (((__len) = fiid_template_block_len_bytes ((__tmpl), (__field_start), (__field_end))) < 0)         \
    {                                                                                                   \
      __FIID_TRACE;                                                                                     \
      return (-1);                                                                                      \
    }                                                                                                   \
} while (0)

#define FIID_TEMPLATE_COMPARE(__tmpl1, __tmpl2)                              \
do {                                                                         \
    int __ret;                                                               \
    if ((__ret = fiid_template_compare ((__tmpl1), (__tmpl2))) < 0)          \
      {                                                                      \
        __FIID_TRACE;                                                        \
        return (-1);                                                         \
      }                                                                      \
    if (!__ret)                                                              \
      {                                                                      \
	errno = EINVAL;                                                      \
        __FIID_TRACE;                                                        \
	return (-1);                                                         \
      }                                                                      \
} while (0)

#define FIID_TEMPLATE_COMPARE_CLEANUP(__tmpl1, __tmpl2)                      \
do {                                                                         \
    int __ret;                                                               \
    if ((__ret = fiid_template_compare ((__tmpl1), (__tmpl2))) < 0)          \
      {                                                                      \
        __FIID_TRACE;                                                        \
        goto cleanup;                                                        \
      }                                                                      \
    if (!__ret)                                                              \
      {                                                                      \
	errno = EINVAL;                                                      \
        __FIID_TRACE;                                                        \
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
  if (!((__obj) = fiid_obj_create(__tmpl)))           \
    {                                                 \
      __FIID_TRACE;                                   \
      return (-1);                                    \
    }                                                 \
} while (0)

#define FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)        \
do {                                                  \
  if (!((__obj) = fiid_obj_create(__tmpl)))           \
    {                                                 \
      __FIID_TRACE;                                   \
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
         __FIID_OBJ_TRACE((__obj_src));                 \
         __FIID_OBJ_SET_ERRNO((__obj_src));             \
         return (-1);                                   \
       }                                                \
} while (0)

#define FIID_OBJ_DUP_CLEANUP(__obj_dest, __obj_src)     \
do {                                                    \
     if (!((__obj_dest) = fiid_obj_dup((__obj_src))))   \
       {                                                \
         __FIID_OBJ_TRACE((__obj_src));                 \
         __FIID_OBJ_SET_ERRNO((__obj_src));             \
         goto cleanup;                                  \
       }                                                \
} while (0)

#define FIID_OBJ_COPY(__obj_dest, __obj_src, __alt_tmpl)            \
do {                                                                \
    if (!((__obj_dest) = fiid_obj_copy((__obj_src),(__alt_tmpl))))  \
       {                                                            \
         __FIID_OBJ_TRACE((__obj_src));                             \
         __FIID_OBJ_SET_ERRNO((__obj_src));                         \
         return (-1);                                               \
       }                                                            \
} while (0)

#define FIID_OBJ_COPY_CLEANUP(__obj_dest, __obj_src, __alt_tmpl)    \
do {                                                                \
    if (!((__obj_dest) = fiid_obj_copy((__obj_src), (__alt_tmpl)))) \
       {                                                            \
         __FIID_OBJ_TRACE((__obj_src));                             \
         __FIID_OBJ_SET_ERRNO((__obj_src));                         \
         goto cleanup;                                              \
       }                                                            \
} while (0)

#define FIID_OBJ_LEN(__len, __obj)                       \
do {                                                     \
    if (((__len) = fiid_obj_len ((__obj))) < 0)          \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_LEN_BYTES(__len, __obj)                 \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_LEN_BYTES_CLEANUP(__len, __obj)         \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         goto cleanup;                                   \
      }                                                  \
} while (0)

#define FIID_OBJ_FIELD_LEN(__len, __obj, __field)                        \
do {                                                                     \
    if (((__len) = fiid_obj_field_len ((__obj), (__field))) < 0)         \
      {                                                                  \
         __FIID_OBJ_TRACE((__obj));                                      \
         __FIID_OBJ_SET_ERRNO((__obj));                                  \
         return (-1);                                                    \
      }                                                                  \
} while (0)

#define FIID_OBJ_FIELD_LEN_BYTES(__len, __obj, __field)                  \
do {                                                                     \
    if (((__len) = fiid_obj_field_len_bytes ((__obj), (__field))) < 0)   \
      {                                                                  \
         __FIID_OBJ_TRACE((__obj));                                      \
         __FIID_OBJ_SET_ERRNO((__obj));                                  \
         return (-1);                                                    \
      }                                                                  \
} while (0)

#define FIID_OBJ_FIELD_LEN_BYTES_CLEANUP(__len, __obj, __field)          \
do {                                                                     \
    if (((__len) = fiid_obj_field_len_bytes ((__obj), (__field))) < 0)   \
      {                                                                  \
         __FIID_OBJ_TRACE((__obj));                                      \
         __FIID_OBJ_SET_ERRNO((__obj));                                  \
         goto cleanup;                                                   \
      }                                                                  \
} while (0)

#define FIID_OBJ_BLOCK_LEN(__len, __obj, __field_start, __field_end)                          \
do {                                                                                          \
    if (((__len) = fiid_obj_block_len ((__obj), (__field_start), (__field_end))) < 0)         \
      {                                                                                       \
         __FIID_OBJ_TRACE((__obj));                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                       \
         return (-1);                                                                         \
      }                                                                                       \
} while (0)

#define FIID_OBJ_BLOCK_LEN_BYTES(__len, __obj, __field_start, __field_end)                    \
do {                                                                                          \
    if (((__len) = fiid_obj_block_len_bytes ((__obj), (__field_start), (__field_end))) < 0)   \
      {                                                                                       \
         __FIID_OBJ_TRACE((__obj));                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                       \
         return (-1);                                                                         \
      }                                                                                       \
} while (0)

#define FIID_OBJ_BLOCK_LEN_BYTES_CLEANUP(__len, __obj, __field_start, __field_end)            \
do {                                                                                          \
    if (((__len) = fiid_obj_block_len_bytes ((__obj), (__field_start), (__field_end))) < 0)   \
      {                                                                                       \
         __FIID_OBJ_TRACE((__obj));                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                       \
         goto cleanup;                                                                        \
      }                                                                                       \
} while (0)

#define FIID_OBJ_CLEAR(__obj)                            \
do {                                                     \
    if (fiid_obj_clear ((__obj)) < 0)                    \
      {                                                  \
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
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         goto cleanup;                                   \
      }                                                  \
} while (0)

#define FIID_OBJ_CLEAR_FIELD(__obj, __field)             \
do {                                                     \
    if (fiid_obj_clear_field ((__obj), (__field)) < 0)   \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_CLEAR_FIELD_CLEANUP(__obj, __field)     \
do {                                                     \
    if (fiid_obj_clear_field ((__obj), (__field)) < 0)   \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
	 goto cleanup;                                   \
      }                                                  \
} while (0)

#define FIID_OBJ_FIELD_LOOKUP(__obj, __field)                       \
do {                                                                \
    int32_t __ret;                                                  \
    if ((__ret = fiid_obj_field_lookup ((__obj), (__field))) < 0)   \
      {                                                             \
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
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_SET_CLEANUP(__obj, __field, __val)      \
do {                                                     \
    if (fiid_obj_set ((__obj), (__field), (__val)) < 0)  \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
        goto cleanup;                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_SET_DATA(__obj, __field, __data, __data_len)               \
do {                                                                        \
    if (fiid_obj_set_data ((__obj), (__field), (__data), (__data_len)) < 0) \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_DATA_CLEANUP(__obj, __field, __data, __data_len)       \
do {                                                                        \
    if (fiid_obj_set_data ((__obj), (__field), (__data), (__data_len)) < 0) \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_DATA_LEN(__len, __obj, __field, __data, __data_len)                \
do {                                                                                    \
    if (((__len) = fiid_obj_set_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_OBJ_TRACE((__obj));                                                     \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         return (-1);                                                                   \
      }                                                                                 \
} while (0)

#define FIID_OBJ_SET_DATA_LEN_CLEANUP(__len, __obj, __field, __data, __data_len)        \
do {                                                                                    \
    if (((__len) = fiid_obj_set_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_OBJ_TRACE((__obj));                                                     \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         goto cleanup;                                                                  \
      }                                                                                 \
} while (0)

#define FIID_OBJ_SET_ALL(__obj, __data, __data_len)                         \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_CLEANUP(__obj, __data, __data_len)                 \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_LEN(__len, __obj, __data, __data_len)              \
do {                                                                        \
    if (((__len) = fiid_obj_set_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_LEN_CLEANUP(__len, __obj, __data, __data_len)      \
do {                                                                        \
    if (((__len) = fiid_obj_set_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_BLOCK(__obj, __field_start, __field_end, __data, __data_len)                    \
do {                                                                                                 \
    if (fiid_obj_set_block ((__obj), (__field_start), (__field_end), (__data), (__data_len)) < 0)    \
      {                                                                                              \
         __FIID_OBJ_TRACE((__obj));                                                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                                                              \
         return (-1);                                                                                \
      }                                                                                              \
} while (0)

#define FIID_OBJ_SET_BLOCK_CLEANUP(__obj, __field_start, __field_end, __data, __data_len)            \
do {                                                                                                 \
    if (fiid_obj_set_block ((__obj), (__field_start), (__field_end), (__data), (__data_len)) < 0)    \
      {                                                                                              \
         __FIID_OBJ_TRACE((__obj));                                                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                                                              \
         goto cleanup;                                                                               \
      }                                                                                              \
} while (0)

#define FIID_OBJ_SET_BLOCK_LEN(__len, __obj, __field_start, __field_end, __data, __data_len)                  \
do {                                                                                                          \
    if (((__len) = fiid_obj_set_block ((__obj), (__field_start), (__field_end), (__data), (__data_len))) < 0) \
      {                                                                                                       \
         __FIID_OBJ_TRACE((__obj));                                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                                       \
         return (-1);                                                                                         \
      }                                                                                                       \
} while (0)

#define FIID_OBJ_SET_BLOCK_LEN_CLEANUP(__len, __obj, __field_start, __field_end, __data, __data_len)          \
do {                                                                                                          \
    if (((__len) = fiid_obj_set_block ((__obj), (__field_start), (__field_end), (__data), (__data_len))) < 0) \
      {                                                                                                       \
         __FIID_OBJ_TRACE((__obj));                                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                                       \
         goto cleanup;                                                                                        \
      }                                                                                                       \
} while (0)

#define FIID_OBJ_GET(__obj, __field, __val)                             \
do {                                                                    \
    uint64_t __localval = 0, *__localval_ptr;                           \
    int8_t __ret;                                                       \
    __localval_ptr = (__val);                                           \
    if ((__ret = fiid_obj_get ((__obj), (__field), &__localval)) < 0)   \
      {                                                                 \
         __FIID_OBJ_TRACE((__obj));                                     \
         __FIID_OBJ_SET_ERRNO((__obj));                                 \
         return (-1);                                                   \
      }                                                                 \
    if (!__ret)                                                         \
      {                                                                 \
         __FIID_OBJ_NO_DATA_TRACE((__field));                           \
         __FIID_ERRNUM_SET_ERRNO(FIID_ERR_INTERNAL_ERROR);              \
         return (-1);                                                   \
      }                                                                 \
    *__localval_ptr = __localval;                                       \
} while (0)

#define FIID_OBJ_GET_CLEANUP(__obj, __field, __val)                     \
do {                                                                    \
    uint64_t __localval = 0, *__localval_ptr;                           \
    int8_t __ret;                                                       \
    __localval_ptr = (__val);                                           \
    if ((__ret = fiid_obj_get ((__obj), (__field), &__localval)) < 0)   \
      {                                                                 \
         __FIID_OBJ_TRACE((__obj));                                     \
         __FIID_OBJ_SET_ERRNO((__obj));                                 \
         goto cleanup;                                                  \
      }                                                                 \
    if (!__ret)                                                         \
      {                                                                 \
         __FIID_OBJ_NO_DATA_TRACE((__field));                           \
         __FIID_ERRNUM_SET_ERRNO(FIID_ERR_INTERNAL_ERROR);              \
         goto cleanup;                                                  \
      }                                                                 \
    *__localval_ptr = __localval;                                       \
} while (0)

#define FIID_OBJ_GET_DATA(__obj, __field, __data, __data_len)                \
do {                                                                         \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)  \
      {                                                                      \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         return (-1);                                                        \
      }                                                                      \
} while (0)

#define FIID_OBJ_GET_DATA_CLEANUP(__obj, __field, __data, __data_len)        \
do {                                                                         \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)  \
      {                                                                      \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         goto cleanup;                                                       \
      }                                                                      \
} while (0)

#define FIID_OBJ_GET_DATA_LEN(__len, __obj, __field, __data, __data_len)                \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_OBJ_TRACE((__obj));                                                     \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         return (-1);                                                                   \
      }                                                                                 \
} while (0)

#define FIID_OBJ_GET_DATA_LEN_CLEANUP(__len, __obj, __field, __data, __data_len)        \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_OBJ_TRACE((__obj));                                                     \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         goto cleanup;                                                                  \
      }                                                                                 \
} while (0)

#define FIID_OBJ_GET_ALL(__obj, __data, __data_len)                          \
do {                                                                         \
    if (fiid_obj_get_all ((__obj), (__data), (__data_len)) < 0)              \
      {                                                                      \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         return (-1);                                                        \
      }                                                                      \
} while (0)

#define FIID_OBJ_GET_ALL_CLEANUP(__obj, __data, __data_len)                  \
do {                                                                         \
    if (fiid_obj_get_all ((__obj), (__data), (__data_len)) < 0)              \
      {                                                                      \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         goto cleanup;                                                       \
      }                                                                      \
} while (0)

#define FIID_OBJ_GET_ALL_LEN(__len, __obj, __data, __data_len)              \
do {                                                                        \
    if (((__len) = fiid_obj_get_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_GET_ALL_LEN_CLEANUP(__len, __obj, __data, __data_len)      \
do {                                                                        \
    if (((__len) = fiid_obj_get_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_GET_BLOCK(__obj, __field_start, __field_end, __data, __data_len)                    \
do {                                                                                                 \
    if (fiid_obj_get_block ((__obj), (__field_start), (__field_end), (__data), (__data_len)) < 0)    \
      {                                                                                              \
         __FIID_OBJ_TRACE((__obj));                                                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                                                              \
         return (-1);                                                                                \
      }                                                                                              \
} while (0)

#define FIID_OBJ_GET_BLOCK_CLEANUP(__obj, __field_start, __field_end, __data, __data_len)            \
do {                                                                                                 \
    if (fiid_obj_get_block ((__obj), (__field_start), (__field_end), (__data), (__data_len)) < 0)    \
      {                                                                                              \
         __FIID_OBJ_TRACE((__obj));                                                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                                                              \
         goto cleanup;                                                                               \
      }                                                                                              \
} while (0)

#define FIID_OBJ_GET_BLOCK_LEN(__len, __obj, __field_start, __field_end, __data, __data_len)                  \
do {                                                                                                          \
    if (((__len) = fiid_obj_get_block ((__obj), (__field_start), (__field_end), (__data), (__data_len))) < 0) \
      {                                                                                                       \
         __FIID_OBJ_TRACE((__obj));                                                                           \
         __FIID_OBJ_SET_ERRNO((__obj));                                                                       \
         return (-1);                                                                                         \
      }                                                                                                       \
} while (0)

#define FIID_OBJ_GET_BLOCK_LEN_CLEANUP(__len, __obj, __field_start, __field_end, __data, __data_len)          \
do {                                                                                                          \
    if (((__len) = fiid_obj_get_block ((__obj), (__field_start), (__field_end), (__data), (__data_len))) < 0) \
      {                                                                                                       \
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
        __FIID_OBJ_TRACE((__obj));                     \
	__FIID_OBJ_SET_ERRNO((__obj));                 \
        return (-1);                                   \
      }                                                \
    if (!__ret)                                        \
      {                                                \
	errno = EINVAL;                                \
        __FIID_OBJ_TRACE((__obj));                     \
	return (-1);                                   \
      }                                                \
} while (0)

#define FIID_OBJ_TEMPLATE(__ptr, __obj)                \
do {                                                   \
    if (!(__ptr = fiid_obj_template((__obj))))         \
      {                                                \
        __FIID_OBJ_TRACE((__obj));                     \
	__FIID_OBJ_SET_ERRNO((__obj));                 \
        return (-1);                                   \
      }                                                \
} while (0)

#define FIID_OBJ_TEMPLATE_CLEANUP(__ptr, __obj)        \
do {                                                   \
    if (!(__ptr = fiid_obj_template((__obj))))         \
      {                                                \
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
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                              \
         return (-1);                                                \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
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
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                              \
         goto cleanup;                                               \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_ERRNO((__obj));                              \
	goto cleanup;                                                \
      }                                                              \
} while (0)

#define FIID_OBJ_TEMPLATE_COMPARE2(__obj, __tmpl1, __tmpl2)            \
do {                                                                   \
    int __ret1;                                                        \
    int __ret2;                                                        \
    if ((__ret1 = fiid_obj_template_compare ((__obj), (__tmpl1))) < 0) \
      {                                                                \
         __FIID_OBJ_TRACE((__obj));                                    \
         __FIID_OBJ_SET_ERRNO((__obj));                                \
         return (-1);                                                  \
      }                                                                \
    if ((__ret2 = fiid_obj_template_compare ((__obj), (__tmpl2))) < 0) \
      {                                                                \
         __FIID_OBJ_TRACE((__obj));                                    \
         __FIID_OBJ_SET_ERRNO((__obj));                                \
         return (-1);                                                  \
      }                                                                \
    if (!__ret1 && !__ret2)                                            \
      {                                                                \
	errno = EINVAL;                                                \
         __FIID_OBJ_TRACE((__obj));                                    \
         __FIID_OBJ_SET_ERRNO((__obj));                                \
	return (-1);                                                   \
      }                                                                \
} while (0)

#define FIID_OBJ_TEMPLATE_COMPARE2_CLEANUP(__obj, __tmpl1, __tmpl2)    \
do {                                                                   \
    int __ret1;                                                        \
    int __ret2;                                                        \
    if ((__ret1 = fiid_obj_template_compare ((__obj), (__tmpl1))) < 0) \
      {                                                                \
         __FIID_OBJ_TRACE((__obj));                                    \
         __FIID_OBJ_SET_ERRNO((__obj));                                \
         goto cleanup;                                                 \
      }                                                                \
    if ((__ret2 = fiid_obj_template_compare ((__obj), (__tmpl2))) < 0) \
      {                                                                \
         __FIID_OBJ_TRACE((__obj));                                    \
         __FIID_OBJ_SET_ERRNO((__obj));                                \
         goto cleanup;                                                 \
      }                                                                \
    if (!__ret1 && !__ret2)                                            \
      {                                                                \
	errno = EINVAL;                                                \
         __FIID_OBJ_TRACE((__obj));                                    \
         __FIID_OBJ_SET_ERRNO((__obj));                                \
	goto cleanup;                                                  \
      }                                                                \
} while (0)

#define FIID_ITERATOR_CREATE(__iter, __obj)                          \
do {                                                                 \
  if (!((__iter) = fiid_iterator_create((__obj))))                   \
    {                                                                \
      __FIID_OBJ_TRACE((__obj));                                     \
      __FIID_OBJ_SET_ERRNO((__obj));                                 \
      return (-1);                                                   \
    }                                                                \
} while (0)

#define FIID_ITERATOR_KEY(__key, __iter)                             \
do {                                                                 \
  if (!((__key) = fiid_iterator_key((__iter))))                      \
    {                                                                \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      return (-1);                                                   \
    }                                                                \
} while (0)

#define FIID_ITERATOR_KEY_CLEANUP(__key, __iter)                     \
do {                                                                 \
  if (!((__key) = fiid_iterator_key((__iter))))                      \
    {                                                                \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      goto cleanup;                                                  \
    }                                                                \
} while (0)

#define FIID_ITERATOR_FIELD_LEN(__field_len, __iter)                 \
do {                                                                 \
  if (((__field_len) = fiid_iterator_field_len((__iter))) < 0)       \
    {                                                                \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      return (-1);                                                   \
    }                                                                \
} while (0)

#define FIID_ITERATOR_FIELD_LEN_CLEANUP(__field_len, __iter)         \
do {                                                                 \
  if (((__field_len) = fiid_iterator_field_len((__iter))) < 0)       \
    {                                                                \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      goto cleanup;                                                  \
    }                                                                \
} while (0)

#define FIID_ITERATOR_GET(__iter, __val)                             \
do {                                                                 \
  if (fiid_iterator_get((__iter), (__val)) < 0)                      \
    {                                                                \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      return (-1);                                                   \
    }                                                                \
} while (0)

#define FIID_ITERATOR_GET_CLEANUP(__iter, __val)                     \
do {                                                                 \
  if (fiid_iterator_get((__iter), (__val)) < 0)                      \
    {                                                                \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      goto cleanup;                                                  \
    }                                                                \
} while (0)

#define FIID_ITERATOR_GET_DATA(__iter, __data, __data_len)           \
do {                                                                 \
  if (fiid_iterator_get_data((__iter), (__data), (__data_len)) < 0)  \
    {                                                                \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      return (-1);                                                   \
    }                                                                \
} while (0)

#define FIID_ITERATOR_GET_DATA_CLEANUP(__iter, __data, __data_len)   \
do {                                                                 \
  if (fiid_iterator_get_data((__iter), (__data), (__data_len)) < 0)  \
    {                                                                \
      __FIID_ITER_TRACE((__iter));                                   \
      __FIID_ITER_SET_ERRNO((__iter));                               \
      goto cleanup;                                                  \
    }                                                                \
} while (0)

#define FIID_ITERATOR_GET_DATA_LEN(__len, __iter, __data, __data_len)            \
do {                                                                             \
  if (((__len) = fiid_iterator_get_data((__iter), (__data), (__data_len))) < 0)  \
    {                                                                            \
      __FIID_ITER_TRACE((__iter));                                               \
      __FIID_ITER_SET_ERRNO((__iter));                                           \
      return (-1);                                                               \
    }                                                                            \
} while (0)

#define FIID_ITERATOR_GET_DATA_LEN_CLEANUP(__len, __iter, __data, __data_len)    \
do {                                                                             \
  if (((__len) = fiid_iterator_get_data((__iter), (__data), (__data_len))) < 0)  \
    {                                                                            \
      __FIID_ITER_TRACE((__iter));                                               \
      __FIID_ITER_SET_ERRNO((__iter));                                           \
      goto cleanup;                                                              \
    }                                                                            \
} while (0)

#define __FIID_ERRNO_TO_KCS_ERRNUM                                            \
do {                                                                          \
  if (errno == 0)                                                             \
    ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;                                   \
  else if (errno == ENOMEM)                                                   \
    ctx->errnum = IPMI_KCS_CTX_ERR_OUT_OF_MEMORY;                             \
  else                                                                        \
    ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL_ERROR;                            \
} while (0)

#define __FIID_ERRNUM_TO_KCS_ERRNUM(___errnum)                                \
do {                                                                          \
  if ((___errnum) == 0)                                                       \
    ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;                                   \
  else if ((___errnum) == FIID_ERR_OUT_OF_MEMORY)                             \
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
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                     \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_KCS_ERRNUM;                                             \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define KCS_FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)                    \
do {                                                                          \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                     \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_KCS_ERRNUM;                                             \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define KCS_FIID_TEMPLATE_FREE(__tmpl) FIID_TEMPLATE_FREE((__tmpl))

#define KCS_FIID_OBJ_CREATE(__obj, __tmpl)                                    \
do {                                                                          \
  if (!((__obj) = fiid_obj_create(__tmpl)))                                   \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_KCS_ERRNUM;                                             \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define KCS_FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)                            \
do {                                                                          \
  if (!((__obj) = fiid_obj_create(__tmpl)))                                   \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_KCS_ERRNUM;                                             \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define KCS_FIID_OBJ_DESTROY(__obj) FIID_OBJ_DESTROY((__obj))

#define KCS_FIID_OBJ_LEN_BYTES(__len, __obj)             \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_ERRNUM_TO_KCS_ERRNUM((__obj));       \
         return (-1);                                    \
      }                                                  \
} while (0)

#define KCS_FIID_OBJ_TEMPLATE(__ptr, __obj)              \
do {                                                     \
    if (!(__ptr = fiid_obj_template((__obj))))           \
      {                                                  \
        __FIID_OBJ_TRACE((__obj));                       \
        __FIID_OBJ_ERRNUM_TO_KCS_ERRNUM((__obj));        \
        return (-1);                                     \
      }                                                  \
} while (0)

#define KCS_FIID_OBJ_TEMPLATE_CLEANUP(__ptr, __obj)      \
do {                                                     \
    if (!(__ptr = fiid_obj_template((__obj))))           \
      {                                                  \
        __FIID_OBJ_TRACE((__obj));                       \
        __FIID_OBJ_ERRNUM_TO_KCS_ERRNUM((__obj));        \
        goto cleanup;                                    \
      }                                                  \
} while (0)

#define __FIID_ERRNO_TO_SSIF_ERRNUM                                           \
do {                                                                          \
  if (errno == 0)                                                             \
    ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;                                  \
  else if (errno == ENOMEM)                                                   \
    ctx->errnum = IPMI_SSIF_CTX_ERR_OUT_OF_MEMORY;                            \
  else                                                                        \
    ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL_ERROR;                           \
} while (0)

#define __FIID_ERRNUM_TO_SSIF_ERRNUM(___errnum)                               \
do {                                                                          \
  if ((___errnum) == 0)                                                       \
    ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;                                  \
  else if ((___errnum) == FIID_ERR_OUT_OF_MEMORY)                             \
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
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                     \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_SSIF_ERRNUM;                                            \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define SSIF_FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)                   \
do {                                                                          \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                     \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_SSIF_ERRNUM;                                            \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define SSIF_FIID_TEMPLATE_FREE(__tmpl) FIID_TEMPLATE_FREE((__tmpl))

#define SSIF_FIID_OBJ_CREATE(__obj, __tmpl)                                   \
do {                                                                          \
  if (!((__obj) = fiid_obj_create(__tmpl)))                                   \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_SSIF_ERRNUM;                                            \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define SSIF_FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)                           \
do {                                                                          \
  if (!((__obj) = fiid_obj_create(__tmpl)))                                   \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_SSIF_ERRNUM;                                            \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define SSIF_FIID_OBJ_DESTROY(__obj) FIID_OBJ_DESTROY((__obj))

#define SSIF_FIID_OBJ_LEN_BYTES(__len, __obj)            \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_ERRNUM_TO_SSIF_ERRNUM((__obj));      \
         return (-1);                                    \
      }                                                  \
} while (0)

#define SSIF_FIID_OBJ_TEMPLATE(__ptr, __obj)             \
do {                                                     \
    if (!(__ptr = fiid_obj_template((__obj))))           \
      {                                                  \
        __FIID_OBJ_TRACE((__obj));                       \
        __FIID_OBJ_ERRNUM_TO_SSIF_ERRNUM((__obj));       \
        return (-1);                                     \
      }                                                  \
} while (0)

#define SSIF_FIID_OBJ_TEMPLATE_CLEANUP(__ptr, __obj)     \
do {                                                     \
    if (!(__ptr = fiid_obj_template((__obj))))           \
      {                                                  \
        __FIID_OBJ_TRACE((__obj));                       \
        __FIID_OBJ_ERRNUM_TO_SSIF_ERRNUM((__obj));       \
        goto cleanup;                                    \
      }                                                  \
} while (0)

#define __FIID_ERRNO_TO_LOCATE_ERRNUM                                         \
do {                                                                          \
  if (errno == 0)                                                             \
    (*locate_errnum) = IPMI_LOCATE_ERR_SUCCESS;                               \
  else if (errno == ENOMEM)                                                   \
    (*locate_errnum) = IPMI_LOCATE_ERR_OUT_OF_MEMORY;                         \
  else                                                                        \
    (*locate_errnum) = IPMI_LOCATE_ERR_INTERNAL_ERROR;                        \
} while (0)

#define __FIID_ERRNUM_TO_LOCATE_ERRNUM(___errnum)                             \
do {                                                                          \
  if ((___errnum) == 0)                                                       \
    (*locate_errnum) = IPMI_LOCATE_ERR_SUCCESS;                               \
  else if ((___errnum) == FIID_ERR_OUT_OF_MEMORY)                             \
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
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                     \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_LOCATE_ERRNUM;                                          \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define LOCATE_FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)                 \
do {                                                                          \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                     \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_LOCATE_ERRNUM;                                          \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define LOCATE_FIID_TEMPLATE_FIELD_LEN_BYTES(__len, __tmpl, __field)                  \
do {                                                                                  \
  if (((__len) = fiid_template_field_len_bytes ((__tmpl), (__field))) < 0)            \
    {                                                                                 \
      __FIID_TRACE;                                                                   \
      __FIID_ERRNO_TO_LOCATE_ERRNUM;                                                  \
      return (-1);                                                                    \
    }                                                                                 \
} while (0)

#define LOCATE_FIID_TEMPLATE_FIELD_LEN_BYTES_CLEANUP(__len, __tmpl, __field)          \
do {                                                                                  \
  if (((__len) = fiid_template_field_len_bytes ((__tmpl), (__field))) < 0)            \
    {                                                                                 \
      __FIID_TRACE;                                                                   \
      __FIID_ERRNO_TO_LOCATE_ERRNUM;                                                  \
      goto cleanup;                                                                   \
    }                                                                                 \
} while (0)

#define LOCATE_FIID_OBJ_CREATE(__obj, __tmpl)                                 \
do {                                                                          \
  if (!((__obj) = fiid_obj_create(__tmpl)))                                   \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_LOCATE_ERRNUM;                                          \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define LOCATE_FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)                         \
do {                                                                          \
  if (!((__obj) = fiid_obj_create(__tmpl)))                                   \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_LOCATE_ERRNUM;                                          \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define LOCATE_FIID_OBJ_DESTROY(__obj) FIID_OBJ_DESTROY((__obj))

#define LOCATE_FIID_OBJ_TEMPLATE_COMPARE(__obj, __tmpl)                       \
do {                                                                          \
    int __ret;                                                                \
    if ((__ret = fiid_obj_template_compare ((__obj), (__tmpl))) < 0)          \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         return (-1);                                                         \
      }                                                                       \
    if (!__ret)                                                               \
      {                                                                       \
        __FIID_OBJ_TRACE((__obj));                                            \
        (*locate_errnum) = IPMI_LOCATE_ERR_INTERNAL_ERROR;                    \
	return (-1);                                                          \
      }                                                                       \
} while (0)

#define LOCATE_FIID_OBJ_SET_ALL(__obj, __data, __data_len)                    \
do {                                                                          \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)               \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         return (-1);                                                         \
      }                                                                       \
} while (0)

#define LOCATE_FIID_OBJ_SET_ALL_CLEANUP(__obj, __data, __data_len)            \
do {                                                                          \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)               \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         goto cleanup;                                                        \
      }                                                                       \
} while (0)

#define LOCATE_FIID_OBJ_GET(__obj, __field, __val)                            \
do {                                                                          \
    uint64_t __localval = 0, *__localval_ptr;                                 \
    int8_t __ret;                                                             \
    __localval_ptr = (__val);                                                 \
    if ((__ret = fiid_obj_get ((__obj), (__field), &__localval)) < 0)         \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         return (-1);                                                         \
      }                                                                       \
    if (!__ret)                                                               \
      {                                                                       \
         __FIID_OBJ_NO_DATA_TRACE((__field));                                 \
         (*locate_errnum) = IPMI_LOCATE_ERR_SYSTEM_ERROR;                     \
         return (-1);                                                         \
      }                                                                       \
    *__localval_ptr = __localval;                                             \
} while (0)

#define LOCATE_FIID_OBJ_GET_CLEANUP(__obj, __field, __val)                    \
do {                                                                          \
    uint64_t __localval = 0, *__localval_ptr;                                 \
    int8_t __ret;                                                             \
    __localval_ptr = (__val);                                                 \
    if ((__ret = fiid_obj_get ((__obj), (__field), &__localval)) < 0)         \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         goto cleanup;                                                        \
      }                                                                       \
    if (!__ret)                                                               \
      {                                                                       \
         __FIID_OBJ_NO_DATA_TRACE((__field));                                 \
         (*locate_errnum) = IPMI_LOCATE_ERR_SYSTEM_ERROR;                     \
         goto cleanup;                                                        \
      }                                                                       \
    *__localval_ptr = __localval;                                             \
} while (0)

#define LOCATE_FIID_OBJ_GET_DATA(__obj, __field, __data, __data_len)          \
do {                                                                          \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)   \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         return (-1);                                                         \
      }                                                                       \
} while (0)

#define LOCATE_FIID_OBJ_GET_DATA_CLEANUP(__obj, __field, __data, __data_len)  \
do {                                                                          \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)   \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_LOCATE_ERRNUM((__obj));                         \
         goto cleanup;                                                        \
      }                                                                       \
} while (0)

#define __FIID_ERRNO_TO_SDR_CACHE_ERRNUM                                      \
do {                                                                          \
  if (errno == 0)                                                             \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;                             \
  else if (errno == ENOMEM)                                                   \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_OUT_OF_MEMORY;                       \
  else                                                                        \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_INTERNAL_ERROR;                      \
} while (0)

#define __FIID_ERRNUM_TO_SDR_CACHE_ERRNUM(___errnum)                          \
do {                                                                          \
  if ((___errnum) == 0)                                                       \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;                             \
  else if ((___errnum) == FIID_ERR_OUT_OF_MEMORY)                             \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_OUT_OF_MEMORY;                       \
  else                                                                        \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_INTERNAL_ERROR;                      \
} while (0)

#define __FIID_OBJ_ERRNUM_TO_SDR_CACHE_ERRNUM(___obj)                         \
do {                                                                          \
  int32_t __obj_errnum = fiid_obj_errnum((___obj));                           \
  __FIID_ERRNUM_TO_SDR_CACHE_ERRNUM(__obj_errnum);                            \
} while (0)

#define SDR_CACHE_FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                      \
do {                                                                          \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                     \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_SDR_CACHE_ERRNUM;                                       \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define SDR_CACHE_FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)              \
do {                                                                          \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                     \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_SDR_CACHE_ERRNUM;                                       \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define SDR_CACHE_FIID_TEMPLATE_FREE(__tmpl) FIID_TEMPLATE_FREE((__tmpl))

#define SDR_CACHE_FIID_OBJ_CREATE(__obj, __tmpl)                              \
do {                                                                          \
  if (!((__obj) = fiid_obj_create(__tmpl)))                                   \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_SDR_CACHE_ERRNUM;                                       \
      return (-1);                                                            \
    }                                                                         \
} while (0)

#define SDR_CACHE_FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)                      \
do {                                                                          \
  if (!((__obj) = fiid_obj_create(__tmpl)))                                   \
    {                                                                         \
      __FIID_TRACE;                                                           \
      __FIID_ERRNO_TO_SDR_CACHE_ERRNUM;                                       \
      goto cleanup;                                                           \
    }                                                                         \
} while (0)

#define SDR_CACHE_FIID_OBJ_SET_ALL(__obj, __data, __data_len)                 \
do {                                                                          \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)               \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_SDR_CACHE_ERRNUM((__obj));                      \
         return (-1);                                                         \
      }                                                                       \
} while (0)

#define SDR_CACHE_FIID_OBJ_SET_ALL_CLEANUP(__obj, __data, __data_len)         \
do {                                                                          \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)               \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_SDR_CACHE_ERRNUM((__obj));                      \
         goto cleanup;                                                        \
      }                                                                       \
} while (0)

#define SDR_CACHE_FIID_OBJ_GET(__obj, __field, __val)                         \
do {                                                                          \
    uint64_t __localval = 0, *__localval_ptr;                                 \
    int8_t __ret;                                                             \
    __localval_ptr = (__val);                                                 \
    if ((__ret = fiid_obj_get ((__obj), (__field), &__localval)) < 0)         \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_SDR_CACHE_ERRNUM((__obj));                      \
         return (-1);                                                         \
      }                                                                       \
    if (!__ret)                                                               \
      {                                                                       \
         __FIID_OBJ_NO_DATA_TRACE((__field));                                 \
         ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_IPMI_ERROR;                     \
         return (-1);                                                         \
      }                                                                       \
    *__localval_ptr = __localval;                                             \
} while (0)

#define SDR_CACHE_FIID_OBJ_GET_CLEANUP(__obj, __field, __val)                 \
do {                                                                          \
    uint64_t __localval = 0, *__localval_ptr;                                 \
    int8_t __ret;                                                             \
    __localval_ptr = (__val);                                                 \
    if ((__ret = fiid_obj_get ((__obj), (__field), &__localval)) < 0)         \
      {                                                                       \
         __FIID_OBJ_TRACE((__obj));                                           \
         __FIID_OBJ_ERRNUM_TO_SDR_CACHE_ERRNUM((__obj));                      \
         goto cleanup;                                                        \
      }                                                                       \
    if (!__ret)                                                               \
      {                                                                       \
         __FIID_OBJ_NO_DATA_TRACE((__field));                                 \
         ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_IPMI_ERROR;                     \
         goto cleanup;                                                        \
      }                                                                       \
    *__localval_ptr = __localval;                                             \
} while (0)

#define SDR_CACHE_FIID_OBJ_GET_DATA_LEN(__len, __obj, __field, __data, __data_len)         \
do {                                                                                       \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0)    \
      {                                                                                    \
         __FIID_OBJ_TRACE((__obj));                                                        \
         __FIID_OBJ_ERRNUM_TO_SDR_CACHE_ERRNUM((__obj));                                   \
         return (-1);                                                                      \
      }                                                                                    \
} while (0)

#define SDR_CACHE_FIID_OBJ_GET_DATA_LEN_CLEANUP(__len, __obj, __field, __data, __data_len) \
do {                                                                                       \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0)    \
      {                                                                                    \
         __FIID_OBJ_TRACE((__obj));                                                        \
         __FIID_OBJ_ERRNUM_TO_SDR_CACHE_ERRNUM((__obj));                                   \
         goto cleanup;                                                                     \
      }                                                                                    \
} while (0)

#define SDR_CACHE_FIID_OBJ_DESTROY(__obj) FIID_OBJ_DESTROY((__obj))

#ifdef __cplusplus
}
#endif

#endif /* ipmi-fiid-wrappers.h */
