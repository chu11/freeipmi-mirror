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

#ifndef _IPMI_ERR_WRAPPERS_H
#define	_IPMI_ERR_WRAPPERS_H

#ifdef __cplusplus
extern "C" {
#endif

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

#define ERR_WRAPPER_STR_MAX_LEN 4096

#if defined (IPMI_TRACE)

#define __MSG_TRACE(__msgtracestr, __msgtracenum)                       \
do {                                                                    \
  fprintf (stderr,                                                      \
           "%s: %d: %s: error '%s' (%d)\n",                             \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                     \
           __msgtracestr, __msgtracenum);                               \
  fflush (stderr);                                                      \
} while (0)

#define __ERRNO_TRACE(__errno_orig)                                     \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = __errno_orig;                                      \
  char __errnostr[ERR_WRAPPER_STR_MAX_LEN];                             \
  memset (__errnostr, '\0', ERR_WRAPPER_STR_MAX_LEN);                   \
  strerror_r(__save_errno, __errnostr, ERR_WRAPPER_STR_MAX_LEN);        \
  fprintf (stderr,                                                      \
           "%s: %d: %s: errno '%s' (%d)\n",                             \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                     \
           __errnostr, __save_errno);                                   \
  fflush (stderr);                                                      \
  __errno_orig = __save_errno;                                          \
} while (0)

#define __TRACE_CTX                                                     \
do {                                                                    \
  fprintf (stderr,                                                      \
           "%s: %d: %s: error '%s' (%d)\n",                             \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                     \
           __ctxerrstr, __ctxerrnum);                                   \
  fflush (stderr);                                                      \
} while (0)

#define __LOCATE_TRACE                                                  \
do {                                                                    \
  int __ctxerrnum = *locate_errnum;                                     \
  char *__ctxerrstr = ipmi_locate_strerror(__ctxerrnum);                \
  __TRACE_CTX;                                                          \
} while (0)

#define __SDR_CACHE_TRACE                                               \
do {                                                                    \
  int __ctxerrnum = ipmi_sdr_cache_ctx_errnum(ctx);                     \
  char *__ctxerrstr = ipmi_sdr_cache_ctx_strerror(__ctxerrnum);         \
  __TRACE_CTX;                                                          \
} while (0)

#define __SDR_PARSE_TRACE                                               \
do {                                                                    \
  int __ctxerrnum = ipmi_sdr_parse_ctx_errnum(ctx);                     \
  char *__ctxerrstr = ipmi_sdr_parse_ctx_strerror(__ctxerrnum);         \
  __TRACE_CTX;                                                          \
} while (0)

#define __SEL_PARSE_TRACE                                               \
do {                                                                    \
  int __ctxerrnum = ipmi_sel_parse_ctx_errnum(ctx);                     \
  char *__ctxerrstr = ipmi_sel_parse_ctx_strerror(__ctxerrnum);         \
  __TRACE_CTX;                                                          \
} while (0)

#define __SENSOR_READ_TRACE                                             \
do {                                                                    \
  int __ctxerrnum = ipmi_sensor_read_ctx_errnum(ctx);                   \
  char *__ctxerrstr = ipmi_sensor_read_ctx_strerror(__ctxerrnum);       \
  __TRACE_CTX;                                                          \
} while (0)

#else
#define __MSG_TRACE(__msgtracestr, __msgtracenum)
#define __ERRNO_TRACE(__errno_orig)
#define __LOCATE_TRACE
#define __SDR_CACHE_TRACE
#define __SDR_PARSE_TRACE
#define __SEL_PARSE_TRACE
#define __SENSOR_READ_TRACE
#endif /* IPMI_TRACE */

#define ERR_TRACE(__str, __num)                                         \
do {                                                                    \
  __MSG_TRACE(__str, __num);                                            \
} while (0)

#define ERRNO_TRACE(__errno)                                            \
do {                                                                    \
  __ERRNO_TRACE(__errno);                                               \
} while (0)

#define ERR_LOG(expr)                                                   \
do {                                                                    \
  __ERRNO_TRACE(errno);                                                 \
  expr;                                                                 \
} while (0)   

#define ERR(expr)                                                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERRNO_TRACE(errno);                                             \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_CLEANUP(expr)                                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERRNO_TRACE(errno);                                             \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EXIT(expr)                                                  \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERRNO_TRACE(errno);                                             \
      exit(1);                                                          \
    }                                                                   \
} while (0)

#define ERR_NULL_RETURN(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERRNO_TRACE(errno);                                             \
      return (NULL);                                                    \
    }                                                                   \
} while (0)

#define ERR_VOID_RETURN(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERRNO_TRACE(errno);                                             \
      return;                                                           \
    }                                                                   \
} while (0)

#define ERR_EINVAL(expr)                                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERRNO_TRACE(errno);                                             \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_EINVAL_CLEANUP(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERRNO_TRACE(errno);                                             \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EINVAL_NULL_RETURN(expr)                                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERRNO_TRACE(errno);                                             \
      return (NULL);                                                    \
    }                                                                   \
} while (0)

#define ERR_ENOSPC(expr)                                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENOSPC;                                                   \
      __ERRNO_TRACE(errno);                                             \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_ENOSPC_CLEANUP(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENOSPC;                                                   \
      __ERRNO_TRACE(errno);                                             \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EMSGSIZE(expr)                                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EMSGSIZE;                                                 \
      __ERRNO_TRACE(errno);                                             \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_EMSGSIZE_CLEANUP(expr)                                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EMSGSIZE;                                                 \
      __ERRNO_TRACE(errno);                                             \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __LOCATE_ERRNO_TO_ERRNUM                                        \
do {                                                                    \
  if (errno == 0)                                                       \
    (*locate_errnum) = IPMI_LOCATE_ERR_SUCCESS;                         \
  else if (errno == EPERM)                                              \
    (*locate_errnum) = IPMI_LOCATE_ERR_PERMISSION;                      \
  else if (errno == EACCES)                                             \
    (*locate_errnum) = IPMI_LOCATE_ERR_PERMISSION;                      \
  else if (errno == ENOMEM)                                             \
    (*locate_errnum) = IPMI_LOCATE_ERR_OUT_OF_MEMORY;                   \
  else if (errno == EINVAL)                                             \
    (*locate_errnum) = IPMI_LOCATE_ERR_INTERNAL_ERROR;                  \
  else                                                                  \
    (*locate_errnum) = IPMI_LOCATE_ERR_SYSTEM_ERROR;                    \
} while (0)

#define LOCATE_ERR(expr)                                                \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __LOCATE_ERRNO_TO_ERRNUM;                                       \
        __LOCATE_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define LOCATE_ERR_CLEANUP(expr)                                        \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __LOCATE_ERRNO_TO_ERRNUM;                                       \
        __LOCATE_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define LOCATE_ERRNUM_SET(__errnum)                                     \
  do {                                                                  \
    (*locate_errnum) = (__errnum);                                      \
    __LOCATE_TRACE;                                                     \
  } while (0)

#define __SDR_CACHE_ERRNO_TO_ERRNUM                                       \
do {                                                                      \
  if (errno == 0)                                                         \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SUCCESS;                         \
  else if (errno == ENOSPC)                                               \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_FILESYSTEM;                      \
  else if (errno == EMFILE)                                               \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_FILESYSTEM;                      \
  else if (errno == ENFILE)                                               \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_FILESYSTEM;                      \
  else if (errno == EPERM)                                                \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_PERMISSION;                      \
  else if (errno == EACCES)                                               \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_PERMISSION;                      \
  else if (errno == EISDIR)                                               \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_PERMISSION;                      \
  else if (errno == EROFS)                                                \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_PERMISSION;                      \
  else if (errno == ENOENT)                                               \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST; \
  else if (errno == ENOTDIR)                                              \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST; \
  else if (errno == ENAMETOOLONG)                                         \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_FILENAME_INVALID;                \
  else if (errno == ELOOP)                                                \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_FILENAME_INVALID;                \
  else if (errno == ENOMEM)                                               \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_OUT_OF_MEMORY;                   \
  else if (errno == EINVAL)                                               \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_INTERNAL_ERROR;                  \
  else                                                                    \
    ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SYSTEM_ERROR;                    \
} while (0)

#define SDR_CACHE_ERR(expr)                                             \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __SDR_CACHE_ERRNO_TO_ERRNUM;                                    \
        __SDR_CACHE_TRACE;                                              \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_CLEANUP(expr)                                     \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __SDR_CACHE_ERRNO_TO_ERRNUM;                                    \
        __SDR_CACHE_TRACE;                                              \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERRNUM_SET(__errnum)                                  \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __SDR_CACHE_TRACE;                                                  \
  } while (0)

#define SDR_PARSE_ERRNUM_SET(__errnum)                                  \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __SDR_PARSE_TRACE;                                                  \
  } while (0)

#define SEL_PARSE_ERRNUM_SET(__errnum)                                  \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __SEL_PARSE_TRACE;                                                  \
  } while (0)

#define SENSOR_READ_ERRNUM_SET(__errnum)                                \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __SENSOR_READ_TRACE;                                                \
  } while (0)

#ifdef __cplusplus
}
#endif

#endif /* ipmi-err-wrappers.h */

