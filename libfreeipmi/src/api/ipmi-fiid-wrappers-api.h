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

#include "ipmi-err-wrappers-api.h"

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

#define API_FIID_TEMPLATE_FREE(__tmpl)   FIID_TEMPLATE_FREE((__tmpl))

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

#define API_FIID_OBJECT_ERROR_TO_API_ERRNUM(__ctx, __obj)                \
do {                                                                     \
  ipmi_set_api_errnum_by_fiid_object((__ctx), (__obj));                  \
  __API_MSG_TRACE(fiid_obj_errormsg((__obj)), fiid_obj_errnum((__obj))); \
} while (0)   

void ipmi_set_api_errnum_by_fiid_object(ipmi_ctx_t ctx, fiid_obj_t obj);

int api_fiid_obj_template_compare (ipmi_ctx_t ctx, fiid_obj_t obj, fiid_template_t tmpl);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-fiid-wrappers-api.h */
