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

#ifndef _IPMI_FIID_WRAPPERS_API_H
#define	_IPMI_FIID_WRAPPERS_API_H 1

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

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#define __FIID_ERRNO_SET_API_ERRNUM                \
do {                                               \
  if (errno == 0)                                  \
    ctx->errnum = IPMI_ERR_SUCCESS;                \
  else if (errno == EINVAL)                        \
    ctx->errnum = IPMI_ERR_PARAMETERS;             \
  else if (errno == ENOMEM)                        \
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;          \
  else                                             \
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;         \
} while (0)

#define __FIID_ERRNUM_SET_API_ERRNUM(___errnum)    \
do {                                               \
  if ((___errnum) == FIID_ERR_SUCCESS)             \
    ctx->errnum = IPMI_ERR_SUCCESS;                \
  else if ((___errnum) == FIID_ERR_OUT_OF_MEMORY)  \
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;          \
  else                                             \
    ctx->errnum = IPMI_ERR_LIBRARY_ERROR;          \
} while (0)

#define __FIID_OBJ_SET_API_ERRNUM(___obj)          \
do {                                               \
  int32_t __objerrnum = fiid_obj_errnum((___obj)); \
  __FIID_ERRNUM_SET_API_ERRNUM(__objerrnum);       \
} while (0)

#define API_FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                       \
do {                                                                     \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                \
    {                                                                    \
      __FIID_TRACE;                                                      \
      __FIID_ERRNO_SET_API_ERRNUM;                                       \
      return (-1);                                                       \
    }                                                                    \
} while (0)

#define API_FIID_TEMPLATE_LEN_BYTES_CLEANUP(__len, __tmpl)               \
do {                                                                     \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                \
    {                                                                    \
      __FIID_TRACE;                                                      \
      __FIID_ERRNO_SET_API_ERRNUM;                                       \
      goto cleanup;                                                      \
    }                                                                    \
} while (0)

#define API_FIID_TEMPLATE_FREE(__tmpl)   FIID_TEMPLATE_FREE((__tmpl))

#define API_FIID_OBJ_CREATE(__obj, __tmpl)                  \
do {                                                        \
  if (!((__obj) = fiid_obj_create((__tmpl))))               \
    {                                                       \
      __FIID_TRACE;                                         \
      __FIID_ERRNO_SET_API_ERRNUM;                          \
      return (-1);                                          \
    }                                                       \
} while (0)

#define API_FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)          \
do {                                                        \
  if (!((__obj) = fiid_obj_create((__tmpl))))               \
    {                                                       \
      __FIID_TRACE;                                         \
      __FIID_ERRNO_SET_API_ERRNUM;                          \
      goto cleanup;                                         \
    }                                                       \
} while (0)

#define API_FIID_OBJ_DESTROY(__obj) FIID_OBJ_DESTROY(__obj)

#define API_FIID_OBJ_LEN_BYTES(__len, __obj)             \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_API_ERRNUM((__obj));             \
         return (-1);                                    \
      }                                                  \
} while (0)

#define API_FIID_OBJ_LEN_BYTES_CLEANUP(__len, __obj)     \
do {                                                     \
    if (((__len) = fiid_obj_len_bytes ((__obj))) < 0)    \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_API_ERRNUM((__obj));             \
         goto cleanup;                                   \
      }                                                  \
} while (0)

#define API_FIID_OBJ_CLEAR(__obj)                        \
do {                                                     \
    if (fiid_obj_clear ((__obj)) < 0)                    \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_API_ERRNUM((__obj));             \
         return (-1);                                    \
      }                                                  \
} while (0)

#define API_FIID_OBJ_CLEAR_FIELD(__obj, __field)         \
do {                                                     \
    if (fiid_obj_clear_field ((__obj), (__field)) < 0)   \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_API_ERRNUM((__obj));             \
         return (-1);                                    \
      }                                                  \
} while (0)

#define API_FIID_OBJ_CLEAR_FIELD_CLEANUP(__obj, __field) \
do {                                                     \
    if (fiid_obj_clear_field ((__obj), (__field)) < 0)   \
      {                                                  \
         __FIID_OBJ_TRACE((__obj));                      \
         __FIID_OBJ_SET_API_ERRNUM((__obj));             \
         goto cleanup;                                   \
      }                                                  \
} while (0)

#define API_FIID_OBJ_GET(__obj, __field, __val)                         \
do {                                                                    \
    uint64_t __localval = 0, *__localval_ptr;                           \
    int8_t __ret;                                                       \
    __localval_ptr = (__val);                                           \
    if ((__ret = fiid_obj_get ((__obj), (__field), &__localval)) < 0)   \
      {                                                                 \
         __FIID_OBJ_TRACE((__obj));                                     \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                            \
         return (-1);                                                   \
      }                                                                 \
    if (!__ret)                                                         \
      {                                                                 \
         __FIID_OBJ_TRACE((__obj));                                     \
         ctx->errnum = IPMI_ERR_IPMI_ERROR;                             \
         return (-1);                                                   \
      }                                                                 \
    *__localval_ptr = __localval;                                       \
} while (0)

#define API_FIID_OBJ_GET_CLEANUP(__obj, __field, __val)                 \
do {                                                                    \
    uint64_t __localval = 0, *__localval_ptr;                           \
    int8_t __ret;                                                       \
    __localval_ptr = (__val);                                           \
    if ((__ret = fiid_obj_get ((__obj), (__field), &__localval)) < 0)   \
      {                                                                 \
         __FIID_OBJ_TRACE((__obj));                                     \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                            \
         goto cleanup;                                                  \
      }                                                                 \
    if (!__ret)                                                         \
      {                                                                 \
         __FIID_OBJ_TRACE((__obj));                                     \
         ctx->errnum = IPMI_ERR_IPMI_ERROR;                             \
         goto cleanup;                                                  \
      }                                                                 \
    *__localval_ptr = __localval;                                       \
} while (0)

#define API_FIID_OBJ_SET(__obj, __field, __val)               \
do {                                                          \
    if (fiid_obj_set ((__obj), (__field), (__val)) < 0)       \
      {                                                       \
         __FIID_OBJ_TRACE((__obj));                           \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                  \
         return (-1);                                         \
      }                                                       \
} while (0)

#define API_FIID_OBJ_SET_CLEANUP(__obj, __field, __val)       \
do {                                                          \
    if (fiid_obj_set ((__obj), (__field), (__val)) < 0)       \
      {                                                       \
         __FIID_OBJ_TRACE((__obj));                           \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                  \
         goto cleanup;                                        \
      }                                                       \
} while (0)

#define API_FIID_OBJ_SET_DATA(__obj, __field, __data, __data_len)           \
do {                                                                        \
    if (fiid_obj_set_data ((__obj), (__field), (__data), (__data_len)) < 0) \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define API_FIID_OBJ_SET_DATA_CLEANUP(__obj, __field, __data, __data_len)   \
do {                                                                        \
    if (fiid_obj_set_data ((__obj), (__field), (__data), (__data_len)) < 0) \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define API_FIID_OBJ_GET_DATA(__obj, __field, __data, __data_len)            \
do {                                                                         \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)  \
      {                                                                      \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                 \
         return (-1);                                                        \
      }                                                                      \
} while (0)

#define API_FIID_OBJ_GET_DATA_CLEANUP(__obj, __field, __data, __data_len)    \
do {                                                                         \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)  \
      {                                                                      \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                 \
         goto cleanup;                                                       \
      }                                                                      \
} while (0)

#define API_FIID_OBJ_GET_DATA_LEN(__len, __obj, __field, __data, __data_len)            \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_OBJ_TRACE((__obj));                                                     \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                            \
         return (-1);                                                                   \
      }                                                                                 \
} while (0)

#define API_FIID_OBJ_GET_DATA_LEN_CLEANUP(__len, __obj, __field, __data, __data_len)    \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_OBJ_TRACE((__obj));                                                     \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                            \
         goto cleanup;                                                                  \
      }                                                                                 \
} while (0)

#define API_FIID_OBJ_SET_ALL(__obj, __data, __data_len)                     \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define API_FIID_OBJ_SET_ALL_CLEANUP(__obj, __data, __data_len)             \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define API_FIID_OBJ_GET_ALL(__obj, __data, __data_len)                      \
do {                                                                         \
    if (fiid_obj_get_all ((__obj), (__data), (__data_len)) < 0)              \
      {                                                                      \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                 \
         return (-1);                                                        \
      }                                                                      \
} while (0)

#define API_FIID_OBJ_GET_ALL_CLEANUP(__obj, __data, __data_len)              \
do {                                                                         \
    if (fiid_obj_get_all ((__obj), (__data), (__data_len)) < 0)              \
      {                                                                      \
         __FIID_OBJ_TRACE((__obj));                                          \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                 \
         goto cleanup;                                                       \
      }                                                                      \
} while (0)

#define API_FIID_OBJ_GET_ALL_LEN(__len, __obj, __data, __data_len)          \
do {                                                                        \
    if (((__len) = fiid_obj_get_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define API_FIID_OBJ_GET_ALL_LEN_CLEANUP(__len, __obj, __data, __data_len)  \
do {                                                                        \
    if (((__len) = fiid_obj_get_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_OBJ_TRACE((__obj));                                         \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                                \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define API_FIID_OBJ_PACKET_VALID(__obj)               \
do {                                                   \
    int __ret;                                         \
    if ((__ret = fiid_obj_packet_valid((__obj))) < 0)  \
      {                                                \
        __FIID_OBJ_TRACE((__obj));                     \
	__FIID_OBJ_SET_API_ERRNUM((__obj));            \
        return (-1);                                   \
      }                                                \
    if (!__ret)                                        \
      {                                                \
        __FIID_OBJ_TRACE((__obj));                     \
        ctx->errnum = IPMI_ERR_PARAMETERS;             \
	return (-1);                                   \
      }                                                \
} while (0)

#define API_FIID_OBJ_TEMPLATE_CLEANUP(__ptr, __obj)    \
do {                                                   \
    if (!(__ptr = fiid_obj_template((__obj))))         \
      {                                                \
        __FIID_OBJ_TRACE((__obj));                     \
	__FIID_OBJ_SET_API_ERRNUM((__obj));            \
        goto cleanup;                                  \
      }                                                \
} while (0)

#define API_FIID_OBJ_TEMPLATE_COMPARE(__obj, __tmpl)                 \
do {                                                                 \
    int __ret;                                                       \
    if ((__ret = fiid_obj_template_compare ((__obj), (__tmpl))) < 0) \
      {                                                              \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                         \
         return (-1);                                                \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
        __FIID_OBJ_TRACE((__obj));                                   \
	/* set via errno */                                          \
	__FIID_ERRNO_SET_API_ERRNUM;                                 \
	return (-1);                                                 \
      }                                                              \
} while (0)

#define API_FIID_OBJ_TEMPLATE_COMPARE_CLEANUP(__obj, __tmpl)         \
do {                                                                 \
    int __ret;                                                       \
    if ((__ret = fiid_obj_template_compare ((__obj), (__tmpl))) < 0) \
      {                                                              \
         __FIID_OBJ_TRACE((__obj));                                  \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                         \
         goto cleanup;                                               \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
        __FIID_OBJ_TRACE((__obj));                                   \
	/* set via errno */                                          \
	__FIID_ERRNO_SET_API_ERRNUM;                                 \
	goto cleanup;                                                \
      }                                                              \
} while (0)

#define API_FIID_OBJ_TEMPLATE_COMPARE2(__obj, __tmpl1, __tmpl2)        \
do {                                                                   \
    int __ret1;                                                        \
    int __ret2;                                                        \
    if ((__ret1 = fiid_obj_template_compare ((__obj), (__tmpl1))) < 0) \
      {                                                                \
         __FIID_OBJ_TRACE((__obj));                                    \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                           \
         return (-1);                                                  \
      }                                                                \
    if ((__ret2 = fiid_obj_template_compare ((__obj), (__tmpl2))) < 0) \
      {                                                                \
         __FIID_OBJ_TRACE((__obj));                                    \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                           \
         return (-1);                                                  \
      }                                                                \
    if (!__ret1 && !__ret2)                                            \
      {                                                                \
	errno = EINVAL;                                                \
        __FIID_OBJ_TRACE((__obj));                                     \
	/* set via errno */                                            \
	__FIID_ERRNO_SET_API_ERRNUM;                                   \
	return (-1);                                                   \
      }                                                                \
} while (0)

#define API_FIID_OBJ_TEMPLATE_COMPARE2_CLEANUP(__obj, __tmpl1, __tmpl2)\
do {                                                                   \
    int __ret1;                                                        \
    int __ret2;                                                        \
    if ((__ret1 = fiid_obj_template_compare ((__obj), (__tmpl1))) < 0) \
      {                                                                \
         __FIID_OBJ_TRACE((__obj));                                    \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                           \
         goto cleanup;                                                 \
      }                                                                \
    if ((__ret2 = fiid_obj_template_compare ((__obj), (__tmpl2))) < 0) \
      {                                                                \
         __FIID_OBJ_TRACE((__obj));                                    \
         __FIID_OBJ_SET_API_ERRNUM((__obj));                           \
         goto cleanup;                                                 \
      }                                                                \
    if (!__ret1 && !__ret2)                                            \
      {                                                                \
	errno = EINVAL;                                                \
        __FIID_OBJ_TRACE((__obj));                                     \
	/* set via errno */                                            \
	__FIID_ERRNO_SET_API_ERRNUM;                                   \
	goto cleanup;                                                  \
      }                                                                \
} while (0)

/* Special one used for debug dumping */
#define API_FIID_OBJ_GET_NO_RETURN(__obj, __field, __val)             \
do {                                                                  \
    uint64_t __localval = 0, *__localval_ptr;                         \
    __localval_ptr = (__val);                                         \
    int8_t __rv;                                                      \
    if ((__rv = fiid_obj_get ((__obj), (__field), &__localval)) < 0)  \
      {                                                               \
         __FIID_OBJ_TRACE((__obj));                                   \
         __FIID_OBJ_SET_ERRNO((__obj));                               \
      }                                                               \
    if (__rv > 0)                                                     \
      *__localval_ptr = __localval;                                   \
} while (0)

#ifdef __cplusplus
}
#endif

#endif /* ipmi-fiid-wrappers-api.h */
