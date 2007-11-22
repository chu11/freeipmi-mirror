/* 
   err-wrappers.h - IPMI error handling

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

#ifndef _ERR_WRAPPERS_H
#define	_ERR_WRAPPERS_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <syslog.h>
#include <errno.h>

#include "freeipmi/fiid.h"

#define ERR_WRAPPER_STR_MAX_LEN 4096

#if defined (IPMI_SYSLOG)

#define __SYSLOG_OUTPUT                                                 \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  char __errbuf[ERR_WRAPPER_STR_MAX_LEN];                               \
  char __errnostr[ERR_WRAPPER_STR_MAX_LEN];                             \
  memset (__errnostr, '\0', ERR_WRAPPER_STR_MAX_LEN);                   \
  strerror_r(__save_errno, __errnostr, ERR_WRAPPER_STR_MAX_LEN);        \
  snprintf (__errbuf, ERR_WRAPPER_STR_MAX_LEN,                          \
            "%s: %d: %s: errno %s (%d)\n",                              \
            __FILE__, __LINE__, __PRETTY_FUNCTION__,                    \
            __errnostr, __save_errno);                                  \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), __errbuf);       \
  errno = __save_errno;                                                 \
} while (0)

#define __SYSLOG_CTX_OUTPUT                                             \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  char __errbuf[ERR_WRAPPER_STR_MAX_LEN];                               \
  char __errnostr[ERR_WRAPPER_STR_MAX_LEN];                             \
  memset (__errnostr, '\0', ERR_WRAPPER_STR_MAX_LEN);                   \
  strerror_r(__save_errno, __errnostr, ERR_WRAPPER_STR_MAX_LEN);        \
  snprintf (__errbuf, ERR_WRAPPER_STR_MAX_LEN,                          \
            "%s: %d: %s: errno %s (%d), error %s (%d)",                 \
            __FILE__, __LINE__, __PRETTY_FUNCTION__,                    \
            __errnostr, __save_errno,                                   \
            __ctxerrstr, __ctxerrnum);                                  \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), __errbuf);       \
  errno = __save_errno;                                                 \
} while (0)

#define __ERR_SYSLOG                                                    \
do {                                                                    \
  __SYSLOG_OUTPUT;                                                      \
} while (0)

#define __KCS_SYSLOG                                                    \
do {                                                                    \
  int __ctxerrnum = ctx->errnum;                                        \
  char *__ctxerrstr = ipmi_kcs_ctx_strerror(__ctxerrnum);               \
  __SYSLOG_CTX_OUTPUT;                                                  \
} while (0)

#define __SSIF_SYSLOG                                                   \
do {                                                                    \
  int __ctxerrnum = ctx->errnum;                                        \
  char *__ctxerrstr = ipmi_ssif_ctx_strerror(__ctxerrnum);              \
  __SYSLOG_CTX_OUTPUT;                                                  \
} while (0)

#define __OPENIPMI_SYSLOG                                               \
do {                                                                    \
  int __ctxerrnum = ctx->errnum;                                        \
  char *__ctxerrstr = ipmi_openipmi_ctx_strerror(__ctxerrnum);          \
  __SYSLOG_CTX_OUTPUT;                                                  \
} while (0)
#else
#define __ERR_SYSLOG
#define __KCS_SYSLOG
#define __SSIF_SYSLOG
#define __OPENIPMI_SYSLOG
#endif /* IPMI_SYSLOG */

#if defined (IPMI_TRACE)

#define __TRACE_OUTPUT                                                  \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  char __errnostr[ERR_WRAPPER_STR_MAX_LEN];                             \
  memset (__errnostr, '\0', ERR_WRAPPER_STR_MAX_LEN);                   \
  strerror_r(__save_errno, __errnostr, ERR_WRAPPER_STR_MAX_LEN);        \
  fprintf (stderr,                                                      \
           "%s: %d: %s: errno %s (%d)\n",                               \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                     \
           __errnostr, __save_errno);                                   \
  fflush (stderr);                                                      \
  errno = __save_errno;                                                 \
} while (0)

#define __TRACE_CTX_OUTPUT                                              \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  char __errnostr[ERR_WRAPPER_STR_MAX_LEN];                             \
  memset (__errnostr, '\0', ERR_WRAPPER_STR_MAX_LEN);                   \
  strerror_r(__save_errno, __errnostr, ERR_WRAPPER_STR_MAX_LEN);        \
  fprintf (stderr,                                                      \
           "%s: %d: %s: errno %s (%d), error %s (%d)\n",                \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                     \
           __errnostr, __save_errno,                                    \
           __ctxerrstr, __ctxerrnum);                                   \
  fflush (stderr);                                                      \
  errno = __save_errno;                                                 \
} while (0)

#define __ERR_TRACE                                                     \
do {                                                                    \
  __TRACE_OUTPUT;                                                       \
} while (0)

#define __KCS_TRACE                                                     \
do {                                                                    \
  int __ctxerrnum = ipmi_kcs_ctx_errnum(ctx);                           \
  char *__ctxerrstr = ipmi_kcs_ctx_strerror(__ctxerrnum);               \
  __TRACE_CTX_OUTPUT;                                                   \
} while (0)

#define __SSIF_TRACE                                                    \
do {                                                                    \
  int __ctxerrnum = ipmi_ssif_ctx_errnum(ctx);                          \
  char *__ctxerrstr = ipmi_ssif_ctx_strerror(__ctxerrnum);              \
  __TRACE_CTX_OUTPUT;                                                   \
} while (0)

#define __OPENIPMI_TRACE                                                \
do {                                                                    \
  int __ctxerrnum = ipmi_openipmi_ctx_errnum(ctx);                      \
  char *__ctxerrstr = ipmi_openipmi_ctx_strerror(__ctxerrnum);          \
  __TRACE_CTX_OUTPUT;                                                   \
} while (0)

#else
#define __ERR_TRACE
#define __KCS_TRACE
#define __SSIF_TRACE
#define __OPENIPMI_TRACE
#endif /* IPMI_TRACE */

#define ERR_LOG(expr)                                                   \
do {                                                                    \
  __ERR_SYSLOG;                                                         \
  __ERR_TRACE;                                                          \
  expr;                                                                 \
} while (0)   

#define ERR(expr)                                                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_CLEANUP(expr)                                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EXIT(expr)                                                  \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      exit(1);                                                          \
    }                                                                   \
} while (0)

#define ERR_NULL_RETURN(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return (NULL);                                                    \
    }                                                                   \
} while (0)

#define ERR_VOID_RETURN(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return;                                                           \
    }                                                                   \
} while (0)

#define ERR_NO_RETURN(expr)                                             \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
    }                                                                   \
} while (0)

#define ERR_EINVAL(expr)                                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_EINVAL_CLEANUP(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EINVAL_NULL_RETURN(expr)                                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return (NULL);                                                    \
    }                                                                   \
} while (0)

#define ERR_EINVAL_VOID_RETURN(expr)                                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return;                                                           \
    }                                                                   \
} while (0)

#define ERR_ENOSPC(expr)                                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENOSPC;                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_ENOSPC_CLEANUP(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENOSPC;                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_ENODEV(expr)                                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENODEV;                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_ENODEV_CLEANUP(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENODEV;                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EMSGSIZE(expr)                                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EMSGSIZE;                                                 \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_EMSGSIZE_CLEANUP(expr)                                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EMSGSIZE;                                                 \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __KCS_ERRNO_TO_UDM_ERRNUM                                       \
do {                                                                    \
  if (errno == 0)                                                       \
    ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;                             \
  else if (errno == EINTR)                                              \
    ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;                                \
  else if (errno == EAGAIN)                                             \
    ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;                                \
  else if (errno == EPERM)                                              \
    ctx->errnum = IPMI_KCS_CTX_ERR_PERMISSION;                          \
  else if (errno == EACCES)                                             \
    ctx->errnum = IPMI_KCS_CTX_ERR_PERMISSION;                          \
  else if (errno == ENOENT)                                             \
    ctx->errnum = IPMI_KCS_CTX_ERR_DEVICE_NOT_FOUND;                    \
  else if (errno == ENOMEM)                                             \
    ctx->errnum = IPMI_KCS_CTX_ERR_OUT_OF_MEMORY;                       \
  else if (errno == EINVAL)                                             \
    ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL_ERROR;                      \
  else                                                                  \
    ctx->errnum = IPMI_KCS_CTX_ERR_SYSTEM_ERROR;                        \
} while (0)

#define KCS_ERR(expr)                                                   \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __KCS_ERRNO_TO_UDM_ERRNUM;                                      \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_CLEANUP(expr)                                           \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __KCS_ERRNO_TO_UDM_ERRNUM;                                      \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_LOG(expr)                                                   \
  do {                                                                  \
    expr;                                                               \
    __KCS_SYSLOG;                                                       \
    __KCS_TRACE;                                                        \
  } while (0)

#define KCS_LOG_CLEANUP(expr)                                           \
  do {                                                                  \
    expr;                                                               \
    __KCS_SYSLOG;                                                       \
    __KCS_TRACE;                                                        \
    goto cleanup;                                                       \
  } while (0)

#define KCS_ERRNO

#define KCS_ERR_PARAMETERS(expr)                                        \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;                      \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_PARAMETERS_CLEANUP(expr)                                \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;                      \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_IO_NOT_INITIALIZED(expr)                                \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_IO_NOT_INITIALIZED;              \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_IO_NOT_INITIALIZED_CLEANUP(expr)                        \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_IO_NOT_INITIALIZED;              \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_BUSY(expr)                                              \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;                            \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_BUSY_CLEANUP(expr)                                      \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;                            \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_OUT_OF_MEMORY(expr)                                     \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_OUT_OF_MEMORY;                   \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_OUT_OF_MEMORY_CLEANUP(expr)                             \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_OUT_OF_MEMORY;                   \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_SYSTEM_ERROR(expr)                                      \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_SYSTEM_ERROR;                    \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_SYSTEM_ERROR_CLEANUP(expr)                              \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_SYSTEM_ERROR;                    \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_INTERNAL_ERROR(expr)                                    \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL_ERROR;                  \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_INTERNAL_ERROR_CLEANUP(expr)                            \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL_ERROR;                  \
        __KCS_SYSLOG;                                                   \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#ifdef __cplusplus
}
#endif

#endif /* err-wrappers.h */

