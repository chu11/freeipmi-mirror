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

#define __TRACE_OUTPUT                                                  \
do {                                                                    \
  extern int errno;                                                     \
  int __save_errno = errno;                                             \
  char __errnostr[ERR_WRAPPER_STR_MAX_LEN];                             \
  memset (__errnostr, '\0', ERR_WRAPPER_STR_MAX_LEN);                   \
  strerror_r(__save_errno, __errnostr, ERR_WRAPPER_STR_MAX_LEN);        \
  fprintf (stderr,                                                      \
           "%s: %d: %s: errno %s (%d)\n",                               \
           __FILE__, __LINE__, __FUNCTION__,                            \
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
           __FILE__, __LINE__, __FUNCTION__,                            \
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

#define __SUNBMC_TRACE                                                  \
do {                                                                    \
  int __ctxerrnum = ipmi_sunbmc_ctx_errnum(ctx);                        \
  char *__ctxerrstr = ipmi_sunbmc_ctx_strerror(__ctxerrnum);            \
  __TRACE_CTX_OUTPUT;                                                   \
} while (0)

#define __LOCATE_TRACE                                                  \
do {                                                                    \
  int __ctxerrnum = *locate_errnum;                                     \
  char *__ctxerrstr = ipmi_locate_strerror(__ctxerrnum);                \
  __TRACE_CTX_OUTPUT;                                                   \
} while (0)

#define __SDR_CACHE_TRACE                                               \
do {                                                                    \
  int __ctxerrnum = ipmi_sdr_cache_ctx_errnum(ctx);                     \
  char *__ctxerrstr = ipmi_sdr_cache_ctx_strerror(__ctxerrnum);         \
  __TRACE_CTX_OUTPUT;                                                   \
} while (0)

#else
#define __ERR_TRACE
#define __KCS_TRACE
#define __SSIF_TRACE
#define __OPENIPMI_TRACE
#define __SUNBMC_TRACE
#define __LOCATE_TRACE
#define __SDR_CACHE_TRACE
#endif /* IPMI_TRACE */

#define ERR_LOG(expr)                                                   \
do {                                                                    \
  __ERR_TRACE;                                                          \
  expr;                                                                 \
} while (0)   

#define ERR(expr)                                                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_CLEANUP(expr)                                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EXIT(expr)                                                  \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_TRACE;                                                      \
      exit(1);                                                          \
    }                                                                   \
} while (0)

#define ERR_NULL_RETURN(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_TRACE;                                                      \
      return (NULL);                                                    \
    }                                                                   \
} while (0)

#define ERR_VOID_RETURN(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_TRACE;                                                      \
      return;                                                           \
    }                                                                   \
} while (0)

#define ERR_NO_RETURN(expr)                                             \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_TRACE;                                                      \
    }                                                                   \
} while (0)

#define ERR_EINVAL(expr)                                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_EINVAL_CLEANUP(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EPERM(expr)                                                 \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EPERM;                                                    \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_ENOSPC(expr)                                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENOSPC;                                                   \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_ENOSPC_CLEANUP(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENOSPC;                                                   \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define ERR_EMSGSIZE(expr)                                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EMSGSIZE;                                                 \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_EMSGSIZE_CLEANUP(expr)                                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EMSGSIZE;                                                 \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __KCS_ERRNO_TO_ERRNUM                                           \
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
  else if (errno == ETIMEDOUT)                                          \
    ctx->errnum = IPMI_KCS_CTX_ERR_DRIVER_TIMEOUT;                      \
  else                                                                  \
    ctx->errnum = IPMI_KCS_CTX_ERR_SYSTEM_ERROR;                        \
} while (0)

#define KCS_ERR(expr)                                                   \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __KCS_ERRNO_TO_ERRNUM;                                          \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_CLEANUP(expr)                                           \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __KCS_ERRNO_TO_ERRNUM;                                          \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERRNUM_SET(__errnum)                                        \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __KCS_TRACE;                                                        \
  } while (0)

#define KCS_ERRNUM_SET_CLEANUP(__errnum)                                \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __KCS_TRACE;                                                        \
    goto cleanup;                                                       \
  } while (0)

#define KCS_ERR_PARAMETERS(expr)                                        \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;                      \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_PARAMETERS_CLEANUP(expr)                                \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;                      \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_IO_NOT_INITIALIZED(expr)                                \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_IO_NOT_INITIALIZED;              \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_IO_NOT_INITIALIZED_CLEANUP(expr)                        \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_IO_NOT_INITIALIZED;              \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_BUSY(expr)                                              \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;                            \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_BUSY_CLEANUP(expr)                                      \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;                            \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_OUT_OF_MEMORY(expr)                                     \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_OUT_OF_MEMORY;                   \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_OUT_OF_MEMORY_CLEANUP(expr)                             \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_OUT_OF_MEMORY;                   \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_DRIVER_TIMEOUT(expr)                                    \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_DRIVER_TIMEOUT;                  \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_DRIVER_TIMEOUT_CLEANUP(expr)                            \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_DRIVER_TIMEOUT;                  \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_SYSTEM_ERROR(expr)                                      \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_SYSTEM_ERROR;                    \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_SYSTEM_ERROR_CLEANUP(expr)                              \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_SYSTEM_ERROR;                    \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define KCS_ERR_INTERNAL_ERROR(expr)                                    \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL_ERROR;                  \
        __KCS_TRACE;                                                    \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define KCS_ERR_INTERNAL_ERROR_CLEANUP(expr)                            \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL_ERROR;                  \
        __KCS_TRACE;                                                    \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define __SSIF_ERRNO_TO_ERRNUM                                          \
do {                                                                    \
  if (errno == 0)                                                       \
    ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;                            \
  else if (errno == EINTR)                                              \
    ctx->errnum = IPMI_SSIF_CTX_ERR_BUSY;                               \
  else if (errno == EAGAIN)                                             \
    ctx->errnum = IPMI_SSIF_CTX_ERR_BUSY;                               \
  else if (errno == EPERM)                                              \
    ctx->errnum = IPMI_SSIF_CTX_ERR_PERMISSION;                         \
  else if (errno == EACCES)                                             \
    ctx->errnum = IPMI_SSIF_CTX_ERR_PERMISSION;                         \
  else if (errno == ENOENT)                                             \
    ctx->errnum = IPMI_SSIF_CTX_ERR_DEVICE_NOT_FOUND;                   \
  else if (errno == ENOTDIR)                                            \
    ctx->errnum = IPMI_SSIF_CTX_ERR_DEVICE_NOT_FOUND;                   \
  else if (errno == ENAMETOOLONG)                                       \
    ctx->errnum = IPMI_SSIF_CTX_ERR_DEVICE_NOT_FOUND;                   \
  else if (errno == ENOMEM)                                             \
    ctx->errnum = IPMI_SSIF_CTX_ERR_OUT_OF_MEMORY;                      \
  else if (errno == EINVAL)                                             \
    ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL_ERROR;                     \
  else if (errno == ETIMEDOUT)                                          \
    ctx->errnum = IPMI_SSIF_CTX_ERR_DRIVER_TIMEOUT;                     \
  else                                                                  \
    ctx->errnum = IPMI_SSIF_CTX_ERR_SYSTEM_ERROR;                       \
} while (0)

#define SSIF_ERR(expr)                                                  \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __SSIF_ERRNO_TO_ERRNUM;                                         \
        __SSIF_TRACE;                                                   \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SSIF_ERR_CLEANUP(expr)                                          \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __SSIF_ERRNO_TO_ERRNUM;                                         \
        __SSIF_TRACE;                                                   \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SSIF_ERRNUM_SET(__errnum)                                       \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __KCS_TRACE;                                                        \
  } while (0)

#define SSIF_ERRNUM_SET_CLEANUP(__errnum)                               \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __SSIF_TRACE;                                                       \
    goto cleanup;                                                       \
  } while (0)

#define SSIF_ERR_PARAMETERS(expr)                                       \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;                     \
        __SSIF_TRACE;                                                   \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SSIF_ERR_PARAMETERS_CLEANUP(expr)                               \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;                     \
        __SSIF_TRACE;                                                   \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SSIF_ERR_IO_NOT_INITIALIZED(expr)                               \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SSIF_CTX_ERR_IO_NOT_INITIALIZED;             \
        __SSIF_TRACE;                                                   \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SSIF_ERR_IO_NOT_INITIALIZED_CLEANUP(expr)                       \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SSIF_CTX_ERR_IO_NOT_INITIALIZED;             \
        __SSIF_TRACE;                                                   \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SSIF_ERR_OUT_OF_MEMORY(expr)                                    \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SSIF_CTX_ERR_OUT_OF_MEMORY;                  \
        __SSIF_TRACE;                                                   \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SSIF_ERR_OUT_OF_MEMORY_CLEANUP(expr)                            \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SSIF_CTX_ERR_OUT_OF_MEMORY;                  \
        __SSIF_TRACE;                                                   \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SSIF_ERR_INTERNAL_ERROR(expr)                                   \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL_ERROR;                 \
        __SSIF_TRACE;                                                   \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SSIF_ERR_INTERNAL_ERROR_CLEANUP(expr)                           \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL_ERROR;                 \
        __SSIF_TRACE;                                                   \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define __OPENIPMI_ERRNO_TO_ERRNUM                                      \
do {                                                                    \
  if (errno == 0)                                                       \
    ctx->errnum = IPMI_OPENIPMI_CTX_ERR_SUCCESS;                        \
  else if (errno == EPERM)                                              \
    ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PERMISSION;                     \
  else if (errno == EACCES)                                             \
    ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PERMISSION;                     \
  else if (errno == ENOENT)                                             \
    ctx->errnum = IPMI_OPENIPMI_CTX_ERR_DEVICE_NOT_FOUND;               \
  else if (errno == ENOMEM)                                             \
    ctx->errnum = IPMI_OPENIPMI_CTX_ERR_OUT_OF_MEMORY;                  \
  else if (errno == EINVAL)                                             \
    ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL_ERROR;                 \
  else if (errno == ETIMEDOUT)                                          \
    ctx->errnum = IPMI_OPENIPMI_CTX_ERR_DRIVER_TIMEOUT;                 \
  else                                                                  \
    ctx->errnum = IPMI_OPENIPMI_CTX_ERR_SYSTEM_ERROR;                   \
} while (0)

#define OPENIPMI_ERR(expr)                                              \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __OPENIPMI_ERRNO_TO_ERRNUM;                                     \
        __OPENIPMI_TRACE;                                               \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define OPENIPMI_ERR_CLEANUP(expr)                                      \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __OPENIPMI_ERRNO_TO_ERRNUM;                                     \
        __OPENIPMI_TRACE;                                               \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define OPENIPMI_ERRNUM_SET(__errnum)                                   \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __OPENIPMI_TRACE;                                                   \
  } while (0)

#define OPENIPMI_ERRNUM_SET_CLEANUP(__errnum)                           \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __OPENIPMI_TRACE;                                                   \
    goto cleanup;                                                       \
  } while (0)

#define OPENIPMI_ERR_PARAMETERS(expr)                                   \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PARAMETERS;                 \
        __OPENIPMI_TRACE;                                               \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define OPENIPMI_ERR_PARAMETERS_CLEANUP(expr)                           \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PARAMETERS;                 \
        __OPENIPMI_TRACE;                                               \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define OPENIPMI_ERR_IO_NOT_INITIALIZED(expr)                           \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_IO_NOT_INITIALIZED;         \
        __OPENIPMI_TRACE;                                               \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define OPENIPMI_ERR_IO_NOT_INITIALIZED_CLEANUP(expr)                   \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_IO_NOT_INITIALIZED;         \
        __OPENIPMI_TRACE;                                               \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define OPENIPMI_ERR_OUT_OF_MEMORY(expr)                                \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_OUT_OF_MEMORY;              \
        __OPENIPMI_TRACE;                                               \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define OPENIPMI_ERR_OUT_OF_MEMORY_CLEANUP(expr)                        \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_OUT_OF_MEMORY;              \
        __OPENIPMI_TRACE;                                               \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define OPENIPMI_ERR_INTERNAL_ERROR(expr)                               \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL_ERROR;             \
        __OPENIPMI_TRACE;                                               \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define OPENIPMI_ERR_INTERNAL_ERROR_CLEANUP(expr)                       \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL_ERROR;             \
        __OPENIPMI_TRACE;                                               \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define __SUNBMC_ERRNO_TO_ERRNUM                                        \
do {                                                                    \
  if (errno == 0)                                                       \
    ctx->errnum = IPMI_SUNBMC_CTX_ERR_SUCCESS;                          \
  else if (errno == EPERM)                                              \
    ctx->errnum = IPMI_SUNBMC_CTX_ERR_PERMISSION;                       \
  else if (errno == EACCES)                                             \
    ctx->errnum = IPMI_SUNBMC_CTX_ERR_PERMISSION;                       \
  else if (errno == ENOENT)                                             \
    ctx->errnum = IPMI_SUNBMC_CTX_ERR_DEVICE_NOT_FOUND;                 \
  else if (errno == ENOMEM)                                             \
    ctx->errnum = IPMI_SUNBMC_CTX_ERR_OUT_OF_MEMORY;                    \
  else if (errno == EINVAL)                                             \
    ctx->errnum = IPMI_SUNBMC_CTX_ERR_INTERNAL_ERROR;                   \
  else if (errno == ETIMEDOUT)                                          \
    ctx->errnum = IPMI_SUNBMC_CTX_ERR_DRIVER_TIMEOUT;                   \
  else                                                                  \
    ctx->errnum = IPMI_SUNBMC_CTX_ERR_SYSTEM_ERROR;                     \
} while (0)

#define SUNBMC_ERR(expr)                                                \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __SUNBMC_ERRNO_TO_ERRNUM;                                       \
        __SUNBMC_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SUNBMC_ERR_CLEANUP(expr)                                        \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        __SUNBMC_ERRNO_TO_ERRNUM;                                       \
        __SUNBMC_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SUNBMC_ERRNUM_SET(__errnum)                                     \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __SUNBMC_TRACE;                                                     \
  } while (0)

#define SUNBMC_ERRNUM_SET_CLEANUP(__errnum)                             \
  do {                                                                  \
    ctx->errnum = (__errnum);                                           \
    __SUNBMC_TRACE;                                                     \
    goto cleanup;                                                       \
  } while (0)

#define SUNBMC_ERR_PARAMETERS(expr)                                     \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SUNBMC_CTX_ERR_PARAMETERS;                   \
        __SUNBMC_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SUNBMC_ERR_PARAMETERS_CLEANUP(expr)                             \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SUNBMC_CTX_ERR_PARAMETERS;                   \
        __SUNBMC_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SUNBMC_ERR_IO_NOT_INITIALIZED(expr)                             \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SUNBMC_CTX_ERR_IO_NOT_INITIALIZED;           \
        __SUNBMC_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SUNBMC_ERR_IO_NOT_INITIALIZED_CLEANUP(expr)                     \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SUNBMC_CTX_ERR_IO_NOT_INITIALIZED;           \
        __SUNBMC_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SUNBMC_ERR_OUT_OF_MEMORY(expr)                                  \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SUNBMC_CTX_ERR_OUT_OF_MEMORY;                \
        __SUNBMC_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SUNBMC_ERR_OUT_OF_MEMORY_CLEANUP(expr)                          \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SUNBMC_CTX_ERR_OUT_OF_MEMORY;                \
        __SUNBMC_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SUNBMC_ERR_SYSTEM_ERROR(expr)                                   \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SUNBMC_CTX_ERR_SYSTEM_ERROR;                 \
        __SUNBMC_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SUNBMC_ERR_SYSTEM_ERROR_CLEANUP(expr)                           \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SUNBMC_CTX_ERR_SYSTEM_ERROR;                 \
        __SUNBMC_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SUNBMC_ERR_INTERNAL_ERROR(expr)                                 \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SUNBMC_CTX_ERR_INTERNAL_ERROR;               \
        __SUNBMC_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SUNBMC_ERR_INTERNAL_ERROR_CLEANUP(expr)                         \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SUNBMC_CTX_ERR_INTERNAL_ERROR;               \
        __SUNBMC_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
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

#define LOCATE_ERRNUM_SET_CLEANUP(__errnum)                             \
  do {                                                                  \
    (*locate_errnum) = (__errnum);                                      \
    __LOCATE_TRACE;                                                     \
    goto cleanup;                                                       \
  } while (0)

#define LOCATE_ERR_PARAMETERS(expr)                                     \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        (*locate_errnum) = IPMI_LOCATE_ERR_PARAMETERS;                  \
        __LOCATE_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define LOCATE_ERR_PARAMETERS_CLEANUP(expr)                             \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        (*locate_errnum) = IPMI_LOCATE_ERR_PARAMETERS;                  \
        __LOCATE_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define LOCATE_ERR_IO_NOT_INITIALIZED(expr)                             \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        (*locate_errnum) = IPMI_LOCATE_ERR_IO_NOT_INITIALIZED;          \
        __LOCATE_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define LOCATE_ERR_IO_NOT_INITIALIZED_CLEANUP(expr)                     \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        (*locate_errnum) = IPMI_LOCATE_ERR_IO_NOT_INITIALIZED;          \
        __LOCATE_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define LOCATE_ERR_OUT_OF_MEMORY(expr)                                  \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        (*locate_errnum) = IPMI_LOCATE_ERR_OUT_OF_MEMORY;               \
        __LOCATE_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define LOCATE_ERR_OUT_OF_MEMORY_CLEANUP(expr)                          \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        (*locate_errnum) = IPMI_LOCATE_ERR_OUT_OF_MEMORY;               \
        __LOCATE_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define LOCATE_ERR_SYSTEM_ERROR(expr)                                   \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        (*locate_errnum) = IPMI_LOCATE_ERR_SYSTEM_ERROR;                \
        __LOCATE_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define LOCATE_ERR_SYSTEM_ERROR_CLEANUP(expr)                           \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        (*locate_errnum) = IPMI_LOCATE_ERR_SYSTEM_ERROR;                \
        __LOCATE_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define LOCATE_ERR_INTERNAL_ERROR(expr)                                 \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        (*locate_errnum) = IPMI_LOCATE_ERR_INTERNAL_ERROR;              \
        __LOCATE_TRACE;                                                 \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define LOCATE_ERR_INTERNAL_ERROR_CLEANUP(expr)                         \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        (*locate_errnum) = IPMI_LOCATE_ERR_INTERNAL_ERROR;              \
        __LOCATE_TRACE;                                                 \
        goto cleanup;                                                   \
      }                                                                 \
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

#define SDR_CACHE_ERR_PARAMETERS(expr)                                  \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_PARAMETERS;                \
        __SDR_CACHE_TRACE;                                              \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_OUT_OF_MEMORY(expr)                               \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_OUT_OF_MEMORY;             \
        __SDR_CACHE_TRACE;                                              \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_OUT_OF_MEMORY_CLEANUP(expr)                       \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_OUT_OF_MEMORY;             \
        __SDR_CACHE_TRACE;                                              \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_CACHE_READ_INITIALIZATION(expr)                   \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_INITIALIZATION; \
        __SDR_CACHE_TRACE;                                              \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_CACHE_READ_INITIALIZATION_CLEANUP(expr)           \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_CACHE_READ_INITIALIZATION; \
        __SDR_CACHE_TRACE;                                              \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_IPMI_ERROR(expr)                                  \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_IPMI_ERROR;                \
        __SDR_CACHE_TRACE;                                              \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_IPMI_ERROR_CLEANUP(expr)                          \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_IPMI_ERROR;                \
        __SDR_CACHE_TRACE;                                              \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_SYSTEM_ERROR(expr)                                \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SYSTEM_ERROR;              \
        __SDR_CACHE_TRACE;                                              \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_SYSTEM_ERROR_CLEANUP(expr)                        \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_SYSTEM_ERROR;              \
        __SDR_CACHE_TRACE;                                              \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_INTERNAL_ERROR(expr)                              \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_INTERNAL_ERROR;            \
        __SDR_CACHE_TRACE;                                              \
        return (-1);                                                    \
      }                                                                 \
  } while (0)

#define SDR_CACHE_ERR_INTERNAL_ERROR_CLEANUP(expr)                      \
  do {                                                                  \
    if (!(expr))                                                        \
      {                                                                 \
        ctx->errnum = IPMI_SDR_CACHE_CTX_ERR_INTERNAL_ERROR;            \
        __SDR_CACHE_TRACE;                                              \
        goto cleanup;                                                   \
      }                                                                 \
  } while (0)

#ifdef __cplusplus
}
#endif

#endif /* ipmi-err-wrappers.h */

