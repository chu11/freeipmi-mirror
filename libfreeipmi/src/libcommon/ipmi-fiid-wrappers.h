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

#define FIID_TEMPLATE_FREE(__tmpl)             \
do {                                           \
  if ((__tmpl))                                \
    {                                          \
      fiid_template_free((__tmpl));            \
      (__tmpl) = NULL;                         \
    }                                          \
} while (0)

#define FIID_OBJ_DESTROY(__obj)                \
do {                                           \
  if ((__obj))                                 \
    {                                          \
      fiid_obj_destroy((__obj));               \
      (__obj) = NULL;                          \
    }                                          \
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

#ifdef __cplusplus
}
#endif

#endif /* ipmi-fiid-wrappers.h */
