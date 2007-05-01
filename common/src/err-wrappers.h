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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

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
#include "freeipmi/ipmi-utils.h"

#define ERR_WRAPPER_STR_MAX_LEN 4096

#if defined (IPMI_SYSLOG)
#define __IPMI_SYSLOG                                                   \
do {                                                                    \
  extern int errno;                                                     \
  int save_errno = errno;                                               \
  char errstr[ERR_WRAPPER_STR_MAX_LEN];                                 \
  snprintf (errstr, ERR_WRAPPER_STR_MAX_LEN,                            \
            "%s: %d: %s: errno (%d): expression failed", __FILE__,      \
            __LINE__, __PRETTY_FUNCTION__, save_errno);                 \
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), errstr);         \
  errno = save_errno;                                                   \
} while (0)
#else
#define __IPMI_SYSLOG
#endif /* IPMI_SYSLOG */

#if defined (IPMI_TRACE)
#define __IPMI_TRACE                                                    \
do {                                                                    \
  extern int errno;                                                     \
  int save_errno = errno;                                               \
  fprintf (stderr,                                                      \
           "%s: %d: %s: errno (%d): expression failed\n", __FILE__,     \
           __LINE__, __PRETTY_FUNCTION__, save_errno);                  \
  fflush (stderr);                                                      \
  errno = save_errno;                                                   \
} while (0)

#define __IPMI_TRACE_ERRMSG_CLEANUP(___dev, ___rs)                      \
do {                                                                    \
  extern int errno;                                                     \
  int save_errno = errno;                                               \
  memset((___dev)->errmsg, '\0', IPMI_ERR_STR_MAX_LEN);                 \
  if (!ipmi_strerror_cmd_r ((___rs),                                    \
		      	    (___dev)->net_fn,                           \
			    (___dev)->errmsg,                           \
			    IPMI_ERR_STR_MAX_LEN))                      \
    fprintf (stderr,                                                    \
	     "%s: %d: %s: errmsg = %s\n", __FILE__,                     \
	     __LINE__, __PRETTY_FUNCTION__, (___dev)->errmsg);          \
  fflush(stderr);                                                       \
  errno = save_errno;                                                   \
  goto cleanup;                                                         \
} while (0) 
#else
#define __IPMI_TRACE
#define __IPMI_TRACE_ERRMSG_CLEANUP(___dev, ___rs)                      \
do {                                                                    \
  extern int errno;                                                     \
  int save_errno = errno;                                               \
  memset((___dev)->errmsg, '\0', IPMI_ERR_STR_MAX_LEN);                 \
  ipmi_strerror_cmd_r ((___rs),                                         \
	     	       (___dev)->net_fn,                                \
		       (___dev)->errmsg,                                \
		       IPMI_ERR_STR_MAX_LEN);                           \
  errno = save_errno;                                                   \
  goto cleanup;                                                         \
} while (0) 

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

#define ERR_ENOTSUP(expr)                                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENOTSUP;                                                  \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_ENOTSUP_CLEANUP(expr)                                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = ENOTSUP;                                                  \
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

#define ERR_IPMI_CMD_CLEANUP(__dev, __lun, __netfn, __rq, __rs)                    \
do {                                                                               \
  int8_t __rv;                                                                     \
  ERR_CLEANUP (!(ipmi_cmd ((__dev),                                                \
                           (__lun),                                                \
                           (__netfn),                                              \
                           (__rq),                                                 \
                           (__rs)) < 0));                                          \
  ERR_CLEANUP (!((__rv = ipmi_check_completion_code_success ((__rs))) < 0));       \
  if (!__rv)                                                                       \
    __IPMI_TRACE_ERRMSG_CLEANUP(__dev, __rs);                                      \
} while (0)

/* EINVAL -> INTERNAL_LIBRARY_ERROR b/c bad inputs should be directly returned
 * to the user as IPMI_ERR_INVALID_PARAMETERS
 */
#define __ERRNO_TO_UDM_ERRNUM                      \
do {                                               \
  if (errno == 0)                                  \
    dev->errnum = IPMI_ERR_SUCCESS;                \
  else if (errno == ENOMEM)                        \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;          \
  else if (errno == ENODEV)                        \
    dev->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;   \
  else if (errno == EINVAL)                        \
    dev->errnum = IPMI_ERR_INTERNAL_LIBRARY_ERROR; \
  else                                             \
    dev->errnum = IPMI_ERR_INTERNAL_ERROR;         \
} while (0)

#define UDM_ERR_ERRNO_TO_UDM_ERRNUM(expr)                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      __ERRNO_TO_UDM_ERRNUM;                                            \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_ERRNO_TO_UDM_ERRNUM_CLEANUP(expr)                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      __ERRNO_TO_UDM_ERRNUM;                                            \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

/* INVALID_PARAMETER -> INTERNAL_LIBRARY_ERROR b/c bad inputs should
 * be directly returned to the user as IPMI_ERR_INVALID_PARAMETERS
 */
#define __KCS_ERRNUM_TO_UDM_ERRNUM                                \
do {                                                              \
  int32_t __errnum = ipmi_kcs_ctx_errnum(dev->io.inband.kcs_ctx); \
  if (__errnum == IPMI_KCS_CTX_ERR_SUCCESS)                       \
    dev->errnum = IPMI_ERR_SUCCESS;                               \
  else if (__errnum == IPMI_KCS_CTX_ERR_OUTMEM)                   \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;                         \
  else if (__errnum == IPMI_KCS_CTX_ERR_PERMISSION)               \
    dev->errnum = IPMI_ERR_PERMISSION;                            \
  else if (__errnum == IPMI_KCS_CTX_ERR_PARAMETERS)               \
    dev->errnum = IPMI_ERR_INTERNAL_LIBRARY_ERROR;                \
  else                                                            \
    dev->errnum = IPMI_ERR_INTERNAL_ERROR;                        \
} while (0)

#define UDM_ERR_KCS_ERRNUM_TO_UDM_ERRNUM(expr)                          \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      __KCS_ERRNUM_TO_UDM_ERRNUM;                                       \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_KCS_ERRNUM_TO_UDM_ERRNUM_CLEANUP(expr)                  \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      __KCS_ERRNUM_TO_UDM_ERRNUM;                                       \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

/* INVALID_PARAMETER -> INTERNAL_LIBRARY_ERROR b/c bad inputs should
 * be directly returned to the user as IPMI_ERR_INVALID_PARAMETERS
 */
#define __SSIF_ERRNUM_TO_UDM_ERRNUM                                 \
do {                                                                \
  int32_t __errnum = ipmi_ssif_ctx_errnum(dev->io.inband.ssif_ctx); \
  if (__errnum == IPMI_SSIF_CTX_ERR_SUCCESS)                        \
    dev->errnum = IPMI_ERR_SUCCESS;                                 \
  else if (__errnum == IPMI_SSIF_CTX_ERR_OUTMEM)                    \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;                           \
  else if (__errnum == IPMI_SSIF_CTX_ERR_PERMISSION)                \
    dev->errnum = IPMI_ERR_PERMISSION;                              \
  else if (__errnum == IPMI_SSIF_CTX_ERR_PARAMETERS)                \
    dev->errnum = IPMI_ERR_INTERNAL_LIBRARY_ERROR;                  \
  else                                                              \
    dev->errnum = IPMI_ERR_INTERNAL_ERROR;                          \
} while (0)

#define UDM_ERR_SSIF_ERRNUM_TO_UDM_ERRNUM(expr)                         \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      __SSIF_ERRNUM_TO_UDM_ERRNUM;                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_SSIF_ERRNUM_TO_UDM_ERRNUM_CLEANUP(expr)                 \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      __SSIF_ERRNUM_TO_UDM_ERRNUM;                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

/* INVALID_PARAMETER -> INTERNAL_LIBRARY_ERROR b/c bad inputs should
 * be directly returned to the user as IPMI_ERR_INVALID_PARAMETERS
 */
#define __OPENIPMI_ERRNUM_TO_UDM_ERRNUM                                     \
do {                                                                        \
  int32_t __errnum = ipmi_openipmi_ctx_errnum(dev->io.inband.openipmi_ctx); \
  if (__errnum == IPMI_OPENIPMI_CTX_ERR_SUCCESS)                            \
    dev->errnum = IPMI_ERR_SUCCESS;                                         \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_OUTMEM)                        \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;                                   \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_PERMISSION)                    \
    dev->errnum = IPMI_ERR_PERMISSION;                                      \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_DEVICE_NOTFOUND)               \
    dev->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;                            \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_PARAMETERS)                    \
    dev->errnum = IPMI_ERR_INTERNAL_LIBRARY_ERROR;                          \
  else                                                                      \
    dev->errnum = IPMI_ERR_INTERNAL_ERROR;                                  \
} while (0)

#define UDM_ERR_OPENIPMI_ERRNUM_TO_UDM_ERRNUM(expr)                     \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      __OPENIPMI_ERRNUM_TO_UDM_ERRNUM;                                  \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_OPENIPMI_ERRNUM_TO_UDM_ERRNUM_CLEANUP(expr)             \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      __OPENIPMI_ERRNUM_TO_UDM_ERRNUM;                                  \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_ALREADY_OPEN(expr)                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_ALREADY_OPEN;                       \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_ALREADY_OPEN_CLEANUP(expr)                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_ALREADY_OPEN;                       \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_NOT_OPEN(expr)                                   \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_NOT_OPEN;                           \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_NOT_OPEN_CLEANUP(expr)                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_NOT_OPEN;                           \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_NOT_SUPPORTED(expr)                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;                      \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_NOT_SUPPORTED_CLEANUP(expr)                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;                      \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_INVALID_PARAMETERS(expr)                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_INVALID_PARAMETERS;                        \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_INVALID_PARAMETERS_CLEANUP(expr)                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_INVALID_PARAMETERS;                        \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_INTERNAL_SYSTEM_ERROR(expr)                             \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_INTERNAL_SYSTEM_ERROR;                     \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_INTERNAL_SYSTEM_ERROR_CLEANUP(expr)                     \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_INTERNAL_SYSTEM_ERROR;                     \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_INTERNAL_LIBRARY_ERROR(expr)                            \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_INTERNAL_LIBRARY_ERROR;                    \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_INTERNAL_LIBRARY_ERROR_CLEANUP(expr)                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_INTERNAL_LIBRARY_ERROR;                    \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_INTERNAL_ERROR(expr)                                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_INTERNAL_ERROR;                            \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_INTERNAL_ERROR_CLEANUP(expr)                            \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_INTERNAL_ERROR;                            \
      __IPMI_SYSLOG;                                                    \
      __IPMI_TRACE;                                                     \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#ifdef __cplusplus
}
#endif

#endif /* err-wrappers.h */

