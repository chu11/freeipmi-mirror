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
#define __KCS_SYSLOG                                                    \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  int __errnum = ipmi_kcs_ctx_errnum(ctx);                              \
  char __errstr[ERR_WRAPPER_STR_MAX_LEN];                               \
  char __errmsg[ERR_WRAPPER_STR_MAX_LEN];                               \
  memset (__errmsg, '\0', ERR_WRAPPER_STR_MAX_LEN);                     \
  strerror_r(__save_errno, __errmsg, ERR_WRAPPER_STR_MAX_LEN);          \
  snprintf (__errstr, ERR_WRAPPER_STR_MAX_LEN,                          \
            "%s: %d: %s: errno %s (%d), error %s (%d)",                 \
            __FILE__, __LINE__, __PRETTY_FUNCTION__,                    \
            __errmsg, __save_errno,                                     \
            ipmi_kcs_ctx_strerror(__errnum), __errnum);                 \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), __errstr);       \
  errno = __save_errno;                                                 \
} while (0)

#define __SSIF_SYSLOG                                                   \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  int __errnum = ipmi_ssif_ctx_errnum(ctx);                             \
  char __errstr[ERR_WRAPPER_STR_MAX_LEN];                               \
  char __errmsg[ERR_WRAPPER_STR_MAX_LEN];                               \
  memset (__errmsg, '\0', ERR_WRAPPER_STR_MAX_LEN);                     \
  strerror_r(__save_errno, __errmsg, ERR_WRAPPER_STR_MAX_LEN);          \
  snprintf (__errstr, ERR_WRAPPER_STR_MAX_LEN,                          \
            "%s: %d: %s: errno %s (%d), error %s (%d)",                 \
            __FILE__, __LINE__, __PRETTY_FUNCTION__,                    \
            __errmsg, __save_errno,                                     \
            ipmi_ssif_ctx_strerror(__errnum), __errnum);                \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), __errstr);       \
  errno = __save_errno;                                                 \
} while (0)

#define __OPENIPMI_SYSLOG                                               \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  int __errnum = ipmi_openipmi_ctx_errnum(ctx);                         \
  char __errstr[ERR_WRAPPER_STR_MAX_LEN];                               \
  char __errmsg[ERR_WRAPPER_STR_MAX_LEN];                               \
  memset (__errmsg, '\0', ERR_WRAPPER_STR_MAX_LEN);                     \
  strerror_r(__save_errno, __errmsg, ERR_WRAPPER_STR_MAX_LEN);          \
  snprintf (__errstr, ERR_WRAPPER_STR_MAX_LEN,                          \
            "%s: %d: %s: errno %s (%d), error %s (%d)",                 \
            __FILE__, __LINE__, __PRETTY_FUNCTION__,                    \
            __errmsg, __save_errno,                                     \
            ipmi_openipmi_ctx_strerror(__errnum), __errnum);            \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), __errstr);       \
  errno = __save_errno;                                                 \
} while (0)

#define __IPMI_SYSLOG                                                   \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  char __errstr[ERR_WRAPPER_STR_MAX_LEN];                               \
  char __errmsg[ERR_WRAPPER_STR_MAX_LEN];                               \
  memset (__errmsg, '\0', ERR_WRAPPER_STR_MAX_LEN);                     \
  strerror_r(__save_errno, __errmsg, ERR_WRAPPER_STR_MAX_LEN);          \
  snprintf (__errstr, ERR_WRAPPER_STR_MAX_LEN,                          \
            "%s: %d: %s: errno %s (%d)\n",                              \
            __FILE__, __LINE__, __PRETTY_FUNCTION__,                    \
            __errmsg, __save_errno);                                    \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), __errstr);       \
  errno = __save_errno;                                                 \
} while (0)
#else
#define __KCS_SYSLOG
#define __SSIF_SYSLOG
#define __OPENIPMI_SYSLOG
#define __IPMI_SYSLOG
#endif /* IPMI_SYSLOG */

#if defined (IPMI_TRACE)
#define __KCS_TRACE                                                     \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  int __errnum = ipmi_kcs_ctx_errnum(ctx);                              \
  char __errmsg[ERR_WRAPPER_STR_MAX_LEN];                               \
  memset (__errmsg, '\0', ERR_WRAPPER_STR_MAX_LEN);                     \
  strerror_r(__save_errno, __errmsg, ERR_WRAPPER_STR_MAX_LEN);          \
  fprintf (stderr,                                                      \
           "%s: %d: %s: errno %s (%d), error %s (%d)",                  \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                     \
           __errmsg, __save_errno,                                      \
           ipmi_kcs_ctx_strerror(__errnum), __errnum);                  \
  fflush (stderr);                                                      \
  errno = __save_errno;                                                 \
} while (0)

#define __SSIF_TRACE                                                    \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  int __errnum = ipmi_ssif_ctx_errnum(ctx);                             \
  char __errmsg[ERR_WRAPPER_STR_MAX_LEN];                               \
  memset (__errmsg, '\0', ERR_WRAPPER_STR_MAX_LEN);                     \
  strerror_r(__save_errno, __errmsg, ERR_WRAPPER_STR_MAX_LEN);          \
  fprintf (stderr,                                                      \
           "%s: %d: %s: errno %s (%d), error %s (%d)",                  \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                     \
           __errmsg, __save_errno,                                      \
           ipmi_ssif_ctx_strerror(__errnum), __errnum);                 \
  fflush (stderr);                                                      \
  errno = __save_errno;                                                 \
} while (0)

#define __OPENIPMI_TRACE                                                \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  int __errnum = ipmi_openipmi_ctx_errnum(ctx);                         \
  char __errmsg[ERR_WRAPPER_STR_MAX_LEN];                               \
  memset (__errmsg, '\0', ERR_WRAPPER_STR_MAX_LEN);                     \
  strerror_r(__save_errno, __errmsg, ERR_WRAPPER_STR_MAX_LEN);          \
  fprintf (stderr,                                                      \
           "%s: %d: %s: errno %s (%d), error %s (%d)",                  \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                     \
           __errmsg, __save_errno,                                      \
           ipmi_openipmi_ctx_strerror(__errnum), __errnum);             \
  fflush (stderr);                                                      \
  errno = __save_errno;                                                 \
} while (0)

#define __IPMI_TRACE                                                    \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  char __errmsg[ERR_WRAPPER_STR_MAX_LEN];                               \
  memset (__errmsg, '\0', ERR_WRAPPER_STR_MAX_LEN);                     \
  strerror_r(__save_errno, __errmsg, ERR_WRAPPER_STR_MAX_LEN);          \
  fprintf (stderr,                                                      \
           "%s: %d: %s: errno %s (%d)\n",                               \
           __FILE__, __LINE__, __PRETTY_FUNCTION__,                     \
           __errmsg, __save_errno);                                     \
  fflush (stderr);                                                      \
  errno = __save_errno;                                                 \
} while (0)
#else
#define __KCS_TRACE
#define __SSIF_TRACE
#define __OPENIPMI_TRACE
#define __IPMI_TRACE
#endif /* IPMI_TRACE */

#define ERR_LOG(expr)                                                   \
do {                                                                    \
  __IPMI_SYSLOG;                                                        \
  __IPMI_TRACE;                                                         \
  expr;                                                                 \
} while (0)   

#define ERR(expr)                                                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_CLEANUP(expr)                                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EXIT(expr)                                                  \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      exit(1);                                                          \
    }                                                                   \
} while (0)

#define ERR_NULL_RETURN(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (NULL);                                                    \
    }                                                                   \
} while (0)

#define ERR_VOID_RETURN(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return;                                                           \
    }                                                                   \
} while (0)

#define ERR_NO_RETURN(expr)                                             \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
    }                                                                   \
} while (0)

#define ERR_EINVAL(expr)                                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_EINVAL_CLEANUP(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EINVAL_NULL_RETURN(expr)                                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (NULL);                                                    \
    }                                                                   \
} while (0)

#define ERR_EINVAL_VOID_RETURN(expr)                                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return;                                                           \
    }                                                                   \
} while (0)

#define ERR_ENOSPC(expr)                                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENOSPC;                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_ENOSPC_CLEANUP(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENOSPC;                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_ENODEV(expr)                                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENODEV;                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_ENODEV_CLEANUP(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENODEV;                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EMSGSIZE(expr)                                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EMSGSIZE;                                                 \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_EMSGSIZE_CLEANUP(expr)                                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EMSGSIZE;                                                 \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
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

