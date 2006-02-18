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

#define FIID_WRAPPER_STR_MAX_LEN 4096

#if defined (IPMI_SYSLOG)
#define __FIID_SYSLOG(___obj)                                                   
do {
  int32_t __obj_errnum = fiid_obj_errnum((___obj));
  char errstr[FIID_WRAPPER_STR_MAX_LEN];                            
  snprintf (errstr, FIID_WRAPPER_STR_MAX_LEN,                       
	    "%s: %d: %s: error = %s", 
	    __FILE__,   
	    __LINE__, 
	    __PRETTY_FUNCTION__, 
	    fiid_strerror(__obj_errnum));              
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), errstr);
} while (0)
#else
#define __FIID_SYSLOG(___obj)
#endif /* IPMI_SYSLOG */

#if defined (IPMI_TRACE)
#define __FIID_TRACE(___obj)    
do {
  int32_t __obj_errnum = fiid_obj_errnum((___obj));
  fprintf (stderr,                                                  
	   "%s: %d: %s: error = %s\n", 
	   __FILE__, 
	   __LINE__, 
	   __PRETTY_FUNCTION__, 
	   fiid_strerror(__obj_errnum));
  fflush (stderr);
} while (0)
#else
#define __FIID_TRACE(___obj)
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

#define FIID_TEMPLATE_FREE_NO_RETURN(__tmpl)   \
do {                                           \
  if ((__tmpl))                                \
    fiid_template_free((__tmpl));              \
} while (0)

#define FIID_OBJ_CREATE(__obj, __tmpl)          \
do {                                            \
  if (!((__obj) = fiid_obj_create(__tmpl)))     \
    return (-1);                                \
} while (0)

#define FIID_OBJ_CREATE_CLEANUP(__obj, __tmpl)  \
do {                                            \
  if (!((__obj) = fiid_obj_create(__tmpl)))     \
    goto cleanup;                               \
} while (0)

#define FIID_OBJ_CREATE_CLEANUP1(__obj, __tmpl) \
do {                                            \
  if (!((__obj) = fiid_obj_create(__tmpl)))     \
    goto cleanup1;                              \
} while (0)

#define FIID_OBJ_CREATE_CLEANUP2(__obj, __tmpl) \
do {                                            \
  if (!((__obj) = fiid_obj_create(__tmpl)))     \
    goto cleanup2;                              \
} while (0)

#define FIID_OBJ_CREATE_CLEANUP3(__obj, __tmpl) \
do {                                            \
  if (!((__obj) = fiid_obj_create(__tmpl)))     \
    goto cleanup3;                              \
} while (0)

#define FIID_OBJ_CREATE_CLEANUP4(__obj, __tmpl) \
do {                                            \
  if (!((__obj) = fiid_obj_create(__tmpl)))     \
    goto cleanup4;                              \
} while (0)

#define FIID_OBJ_DESTROY(__obj)                \
do {                                           \
 if ((__obj))                                  \
   {                                           \
     if (fiid_obj_destroy((__obj)) < 0)        \
       {                                       \
         __FIID_SYSLOG((__obj));               \
         __FIID_TRACE((__obj));                \
         __FIID_OBJ_SET_ERRNO((__obj));        \
         return (-1);                          \
       }                                       \
   }                                           \
} while (0)

#define FIID_OBJ_DESTROY_NO_RETURN(__obj)      \
do {                                           \
  if ((__obj))                                 \
    fiid_obj_destroy((__obj));                 \
} while (0)

#define FIID_OBJ_SET(__obj, __field, __val)              \
do {                                                     \
    if (fiid_obj_set ((__obj), (__field), (__val)) < 0)  \
      {                                                  \
         __FIID_SYSLOG((__obj));                         \
         __FIID_TRACE((__obj));                          \
         __FIID_OBJ_SET_ERRNO((__obj));                  \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_SET_DATA(__obj, __field, __data, __data_len)               \
do {                                                                        \
    if (fiid_obj_set_data ((__obj), (__field), (__data), (__data_len)) < 0) \
      {                                                                     \
         __FIID_SYSLOG((__obj));                                            \
         __FIID_TRACE((__obj));                                             \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_DATA_CLEANUP(__obj, __field, __data, __data_len)       \
do {                                                                        \
    if (fiid_obj_set_data ((__obj), (__field), (__data), (__data_len)) < 0) \
      {                                                                     \
         __FIID_SYSLOG((__obj));                                            \
         __FIID_TRACE((__obj));                                             \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_DATA_LEN(__len, __obj, __field, __data, __data_len)                \
do {                                                                                    \
    if (((__len) = fiid_obj_set_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_SYSLOG((__obj));                                                        \
         __FIID_TRACE((__obj));                                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         return (-1);                                                                   \
      }                                                                                 \
} while (0)

#define FIID_OBJ_SET_DATA_LEN_CLEANUP(__len, __obj, __field, __data, __data_len)        \
do {                                                                                    \
    if (((__len) = fiid_obj_set_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_SYSLOG((__obj));                                                        \
         __FIID_TRACE((__obj));                                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         goto cleanup;                                                                  \
      }                                                                                 \
} while (0)

#define FIID_OBJ_SET_ALL(__obj, __data, __data_len)                         \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_SYSLOG((__obj));                                            \
         __FIID_TRACE((__obj));                                             \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_CLEANUP(__obj, __data, __data_len)                 \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_SYSLOG((__obj));                                            \
         __FIID_TRACE((__obj));                                             \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_CLEANUP1(__obj, __data, __data_len)                \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_SYSLOG((__obj));                                            \
         __FIID_TRACE((__obj));                                             \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup1;                                                     \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_CLEANUP2(__obj, __data, __data_len)                \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_SYSLOG((__obj));                                            \
         __FIID_TRACE((__obj));                                             \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup2;                                                     \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_CLEANUP3(__obj, __data, __data_len)                \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         __FIID_SYSLOG((__obj));                                            \
         __FIID_TRACE((__obj));                                             \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup3;                                                     \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_LEN(__len, __obj, __data, __data_len)              \
do {                                                                        \
    if (((__len) = fiid_obj_set_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_SYSLOG((__obj));                                            \
         __FIID_TRACE((__obj));                                             \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         return (-1);                                                       \
      }                                                                     \
} while (0)

#define FIID_OBJ_SET_ALL_LEN_CLEANUP(__len, __obj, __data, __data_len)      \
do {                                                                        \
    if (((__len) = fiid_obj_set_all ((__obj), (__data), (__data_len))) < 0) \
      {                                                                     \
         __FIID_SYSLOG((__obj));                                            \
         __FIID_TRACE((__obj));                                             \
         __FIID_OBJ_SET_ERRNO((__obj));                                     \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define FIID_OBJ_GET(__obj, __field, __val)                   \
do {                                                          \
    uint64_t __localval = 0, *__localval_ptr;                 \
    __localval_ptr = (__val);                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)   \
      {                                                       \
         __FIID_SYSLOG((__obj));                              \
         __FIID_TRACE((__obj));                               \
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
         __FIID_SYSLOG((__obj));                              \
         __FIID_TRACE((__obj));                               \
         __FIID_OBJ_SET_ERRNO((__obj));                       \
         goto cleanup;                                        \
      }                                                       \
    *__localval_ptr = __localval;                             \
} while (0)

#define FIID_OBJ_GET_CLEANUP1(__obj, __field, __val)          \
do {                                                          \
    uint64_t __localval = 0, *__localval_ptr;                 \
    __localval_ptr = (__val);                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)   \
      {                                                       \
         __FIID_SYSLOG((__obj));                              \
         __FIID_TRACE((__obj));                               \
         __FIID_OBJ_SET_ERRNO((__obj));                       \
         goto cleanup1;                                       \
      }                                                       \
    *__localval_ptr = __localval;                             \
} while (0)

#define FIID_OBJ_GET_CLEANUP2(__obj, __field, __val)          \
do {                                                          \
    uint64_t __localval = 0, *__localval_ptr;                 \
    __localval_ptr = (__val);                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)   \
      {                                                       \
         __FIID_SYSLOG((__obj));                              \
         __FIID_TRACE((__obj));                               \
         __FIID_OBJ_SET_ERRNO((__obj));                       \
         goto cleanup2;                                       \
      }                                                       \
    *__localval_ptr = __localval;                             \
} while (0)

#define FIID_OBJ_GET_CLEANUP3(__obj, __field, __val)          \
do {                                                          \
    uint64_t __localval = 0, *__localval_ptr;                 \
    __localval_ptr = (__val);                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)   \
      {                                                       \
         __FIID_SYSLOG((__obj));                              \
         __FIID_TRACE((__obj));                               \
         __FIID_OBJ_SET_ERRNO((__obj));                       \
         goto cleanup3;                                       \
      }                                                       \
    *__localval_ptr = __localval;                             \
} while (0)

#define FIID_OBJ_GET_CLEANUP4(__obj, __field, __val)          \
do {                                                          \
    uint64_t __localval = 0, *__localval_ptr;                 \
    __localval_ptr = (__val);                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)   \
      {                                                       \
         __FIID_SYSLOG((__obj));                              \
         __FIID_TRACE((__obj));                               \
         __FIID_OBJ_SET_ERRNO((__obj));                       \
         goto cleanup4;                                       \
      }                                                       \
    *__localval_ptr = __localval;                             \
} while (0)

#define FIID_OBJ_GET_DATA(__obj, __field, __data, __data_len)                \
do {                                                                         \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)  \
      {                                                                      \
         __FIID_SYSLOG((__obj));                                             \
         __FIID_TRACE((__obj));                                              \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         return (-1);                                                        \
      }                                                                      \
} while (0)

#define FIID_OBJ_GET_DATA_CLEANUP(__obj, __field, __data, __data_len)        \
do {                                                                         \
    if (fiid_obj_get_data ((__obj), (__field), (__data), (__data_len)) < 0)  \
      {                                                                      \
         __FIID_SYSLOG((__obj));                                             \
         __FIID_TRACE((__obj));                                              \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         goto cleanup;                                                       \
      }                                                                      \
} while (0)

#define FIID_OBJ_GET_DATA_LEN(__len, __obj, __field, __data, __data_len)                \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_SYSLOG((__obj));                                                        \
         __FIID_TRACE((__obj));                                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         return (-1);                                                                   \
      }                                                                                 \
} while (0)

#define FIID_OBJ_GET_DATA_LEN_CLEANUP(__len, __obj, __field, __data, __data_len)        \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         __FIID_SYSLOG((__obj));                                                        \
         __FIID_TRACE((__obj));                                                         \
         __FIID_OBJ_SET_ERRNO((__obj));                                                 \
         goto cleanup;                                                                  \
      }                                                                                 \
} while (0)

#define FIID_OBJ_GET_ALL(__obj, __data, __data_len)                          \
do {                                                                         \
    if (fiid_obj_get_all ((__obj), (__data), (__data_len)) < 0)              \
      {                                                                      \
         __FIID_SYSLOG((__obj));                                             \
         __FIID_TRACE((__obj));                                              \
         __FIID_OBJ_SET_ERRNO((__obj));                                      \
         return (-1);                                                        \
      }                                                                      \
} while (0)

#define FIID_OBJ_PACKET_VALID(__obj)                   \
do {                                                   \
    int __ret;                                         \
    if ((__ret = fiid_obj_packet_valid((__obj))) < 0)  \
      {                                                \
        __FIID_SYSLOG((__obj));                        \
        __FIID_TRACE((__obj));                         \
	__FIID_OBJ_SET_ERRNO((__obj));                 \
        return (-1);                                   \
      }                                                \
    if (!__ret)                                        \
      {                                                \
	errno = EINVAL;                                \
	return (-1);                                   \
      }                                                \
} while (0)

#define FIID_OBJ_TEMPLATE_COMPARE(__obj, __tmpl)                     \
do {                                                                 \
    int __ret;                                                       \
    if ((__ret = fiid_obj_template_compare ((__obj), (__tmpl))) < 0) \
      {                                                              \
         __FIID_SYSLOG((__obj));                                     \
         __FIID_TRACE((__obj));                                      \
         __FIID_OBJ_SET_ERRNO((__obj));                              \
         return (-1);                                                \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
	errno = EINVAL;                                              \
	return (-1);                                                 \
      }                                                              \
} while (0)

#ifdef __cplusplus
}
#endif

#endif /* fiid.h */


