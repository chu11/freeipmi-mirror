/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _TOOL_FIID_UTIL_H
#define _TOOL_FIID_UTIL_H

#include "freeipmi/fiid/fiid.h"

#define TOOL_FIID_OBJ_CREATE(__obj, __tmpl)                                             \
do {                                                                                    \
  if (!((__obj) = fiid_obj_create((__tmpl))))                                           \
    {                                                                                   \
      pstdout_fprintf(state_data->pstate,                                               \
                      stderr,                                                           \
                      "fiid_obj_create: %s\n",                                          \
                      strerror(errno));                                                 \
      goto cleanup;                                                                     \
    }                                                                                   \
} while (0)

#define TOOL_FIID_OBJ_GET(__obj, __field, __val)                                        \
do {                                                                                    \
    uint64_t __localval = 0, *__localval_ptr;                                           \
    int8_t __ret;                                                                       \
    __localval_ptr = (__val);                                                           \
    if ((__ret = fiid_obj_get ((__obj), (__field), &__localval)) < 0)                   \
      {                                                                                 \
        pstdout_fprintf(state_data->pstate,                                             \
                        stderr,                                                         \
                        "fiid_obj_get: %s: %s\n",                                       \
                        __field,                                                        \
                        fiid_obj_errormsg((__obj)));                                    \
        goto cleanup;                                                                   \
      }                                                                                 \
    if (!__ret)                                                                         \
      {                                                                                 \
        pstdout_fprintf(state_data->pstate,                                             \
                        stderr,                                                         \
                        "fiid_obj_get: %s: no data set\n",                              \
                        __field);                                                       \
        goto cleanup;                                                                   \
      }                                                                                 \
    *__localval_ptr = __localval;                                                       \
} while (0)

#define TOOL_FIID_OBJ_GET_WITH_RV(__rv, __obj, __field, __val)                          \
  do {                                                                                  \
    uint64_t __localval = 0, *__localval_ptr;                                           \
    __localval_ptr = (__val);                                                           \
    if (((__rv) = fiid_obj_get ((__obj), (__field), &__localval)) < 0)                  \
      {                                                                                 \
        pstdout_fprintf(state_data->pstate,                                             \
                        stderr,                                                         \
                        "fiid_obj_get: %s: %s\n",                                       \
                        __field,                                                        \
                        fiid_obj_errormsg((__obj)));                                    \
         goto cleanup;                                                                  \
      }                                                                                 \
    *__localval_ptr = __localval;                                                       \
  } while (0)

#define TOOL_FIID_OBJ_GET_DATA(__obj, __field, __data, __datalen)                       \
  do {                                                                                  \
    if (fiid_obj_get_data ((__obj),                                                     \
                           (__field),                                                   \
                           (__data),                                                    \
                           (__datalen)) < 0)                                            \
      {                                                                                 \
        pstdout_fprintf(state_data->pstate,                                             \
                        stderr,                                                         \
                        "fiid_obj_get_data: %s: %s\n",                                  \
                        (__field),                                                      \
                        fiid_obj_errormsg((__obj)));                                    \
        goto cleanup;                                                                   \
      }                                                                                 \
  } while (0)

#define TOOL_FIID_OBJ_GET_DATA_LEN(__len, __obj, __field, __data, __data_len)           \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         pstdout_fprintf(state_data->pstate,                                            \
                         stderr,                                                        \
                         "fiid_obj_get_data: %s: %s\n",                                 \
                         __field,                                                       \
                         fiid_obj_errormsg((__obj)));                                   \
         goto cleanup;                                                                  \
      }                                                                                 \
} while (0)

#define TOOL_FIID_OBJ_DESTROY(__obj)                                                    \
  do {                                                                                  \
    if ((__obj))                                                                        \
      {                                                                                 \
        fiid_obj_destroy((__obj));                                                      \
        (__obj) = NULL;                                                                 \
      }                                                                                 \
  } while (0)

#endif
