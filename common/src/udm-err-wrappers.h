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

#ifndef _UDM_ERR_WRAPPERS_H
#define	_UDM_ERR_WRAPPERS_H

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
#include "freeipmi/ipmi-comp-code-spec.h"
#include "freeipmi/ipmi-utils.h"
#include "freeipmi/udm/ipmi-udm.h"

#include "err-wrappers.h"

#if defined (IPMI_SYSLOG)
#define __UDM_SYSLOG                                                         \
do {                                                                         \
  int __ctxerrnum = dev->errnum;                                             \
  char *__ctxerrstr = ipmi_device_strerror(__ctxerrnum);                     \
  __SYSLOG_CTX_OUTPUT;                                                       \
} while (0)

#define __ERR_UDM_SYSLOG                                                     \
do {                                                                         \
  int __ctxerrnum = ipmi_device_errnum(dev);                                 \
  char *__ctxerrstr = ipmi_device_strerror(__ctxerrnum);                     \
  __SYSLOG_CTX_OUTPUT;                                                       \
} while (0)

#define __UDM_KCS_SYSLOG                                                     \
do {                                                                         \
  int __ctxerrnum = ipmi_kcs_ctx_errnum(dev->io.inband.kcs_ctx);             \
  char *__ctxerrstr = ipmi_kcs_ctx_strerror(__ctxerrnum);                    \
  __SYSLOG_CTX_OUTPUT;                                                       \
} while (0)

#define __UDM_SSIF_SYSLOG                                                    \
do {                                                                         \
  int __ctxerrnum = ipmi_ssif_ctx_errnum(dev->io.inband.ssif_ctx);           \
  char *__ctxerrstr = ipmi_ssif_ctx_strerror(__ctxerrnum);                   \
  __SYSLOG_CTX_OUTPUT;                                                       \
} while (0)

#define __UDM_OPENIPMI_SYSLOG                                                \
do {                                                                         \
  int __ctxerrnum = ipmi_openipmi_ctx_errnum(dev->io.inband.openipmi_ctx);   \
  char *__ctxerrstr = ipmi_openipmi_ctx_strerror(__ctxerrnum);               \
  __SYSLOG_CTX_OUTPUT;                                                       \
} while (0)

#define __UDM_LOCATE_SYSLOG                                                  \
do {                                                                         \
  int __ctxerrnum = __locate_errnum;                                         \
  char *__ctxerrstr = ipmi_locate_strerror(__ctxerrnum);                     \
  __SYSLOG_CTX_OUTPUT;                                                       \
} while (0)
#else
#define __UDM_SYSLOG
#define __ERR_UDM_SYSLOG
#define __UDM_KCS_SYSLOG
#define __UDM_SSIF_SYSLOG
#define __UDM_OPENIPMI_SYSLOG
#define __UDM_LOCATE_SYSLOG
#endif /* IPMI_SYSLOG */

#if defined (IPMI_TRACE)
#define __UDM_TRACE                                                          \
do {                                                                         \
  int __ctxerrnum = dev->errnum;                                             \
  char *__ctxerrstr = ipmi_device_strerror(__ctxerrnum);                     \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)

#define __ERR_UDM_TRACE                                                      \
do {                                                                         \
  int __ctxerrnum = ipmi_device_errnum(dev);                                 \
  char *__ctxerrstr = ipmi_device_strerror(__ctxerrnum);                     \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)

#define __UDM_TRACE_ERRMSG_CLEANUP(___dev, ___rs)                            \
do {                                                                         \
  int __ctxerrnum = 0;                                                       \
  char __ctxerrstr[ERR_WRAPPER_STR_MAX_LEN];                                 \
  memset(__ctxerrstr, '\0', IPMI_ERR_STR_MAX_LEN);                           \
  ipmi_completion_code_strerror_cmd_r ((___rs),                              \
		                       (___dev)->net_fn,                     \
		                       __ctxerrstr,                          \
		                       IPMI_ERR_STR_MAX_LEN);                \
  __TRACE_CTX_OUTPUT;                                                        \
  goto cleanup;                                                              \
} while (0) 

#define __UDM_KCS_TRACE                                                      \
do {                                                                         \
  int __ctxerrnum = ipmi_kcs_ctx_errnum(dev->io.inband.kcs_ctx);             \
  char *__ctxerrstr = ipmi_kcs_ctx_strerror(__ctxerrnum);                    \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)

#define __UDM_SSIF_TRACE                                                     \
do {                                                                         \
  int __ctxerrnum = ipmi_ssif_ctx_errnum(dev->io.inband.ssif_ctx);           \
  char *__ctxerrstr = ipmi_ssif_ctx_strerror(__ctxerrnum);                   \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)

#define __UDM_OPENIPMI_TRACE                                                 \
do {                                                                         \
  int __ctxerrnum = ipmi_openipmi_ctx_errnum(dev->io.inband.openipmi_ctx);   \
  char *__ctxerrstr = ipmi_openipmi_ctx_strerror(__ctxerrnum);               \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)

#define __UDM_LOCATE_TRACE                                                   \
do {                                                                         \
  int __ctxerrnum = __locate_errnum;                                         \
  char *__ctxerrstr = ipmi_locate_strerror(__ctxerrnum);                     \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)
#else
#define __UDM_TRACE
#define __ERR_UDM_TRACE
#define __UDM_TRACE_ERRMSG_CLEANUP(__dev, __rs)                              \
do {                                                                         \
  goto cleanup;                                                              \
} while (0) 
#define __UDM_KCS_TRACE
#define __UDM_SSIF_TRACE
#define __UDM_OPENIPMI_TRACE
#define __UDM_LOCATE_TRACE
#endif /* IPMI_TRACE */

#define __ERRNO_TO_UDM_ERRNUM                      \
do {                                               \
  if (errno == 0)                                  \
    dev->errnum = IPMI_ERR_SUCCESS;                \
  else if (errno == ENOMEM)                        \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;          \
  else if (errno == ENODEV)                        \
    dev->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;   \
  else if (errno == EINVAL)                        \
    dev->errnum = IPMI_ERR_LIBRARY_ERROR;          \
  else                                             \
    dev->errnum = IPMI_ERR_INTERNAL_ERROR;         \
} while (0)

#define UDM_ERR_DEV_CHECK(expr)                                         \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_SET_ERRNUM(__errnum)                                    \
do {                                                                    \
  dev->errnum = (__errnum);                                             \
  __UDM_SYSLOG;                                                         \
  __UDM_TRACE;                                                          \
} while (0)   

#define UDM_ERR_SET_ERRNUM_CLEANUP(__errnum)                            \
do {                                                                    \
  dev->errnum = (__errnum);                                             \
  __UDM_SYSLOG;                                                         \
  __UDM_TRACE;                                                          \
  goto cleanup;                                                         \
} while (0)   

#define UDM_ERR_LOG(expr)                                               \
do {                                                                    \
  __UDM_SYSLOG;                                                         \
  __UDM_TRACE;                                                          \
} while (0)   

#define UDM_ERR_LOG_CLEANUP(expr)                                       \
do {                                                                    \
  __UDM_SYSLOG;                                                         \
  __UDM_TRACE;                                                          \
  goto cleanup;                                                         \
} while (0)   

#define UDM_ERR(expr)                                                   \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERRNO_TO_UDM_ERRNUM;                                            \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_CLEANUP(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERRNO_TO_UDM_ERRNUM;                                            \
      __ERR_SYSLOG;                                                     \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_ALREADY_OPEN(expr)                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_ALREADY_OPEN;                       \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_ALREADY_OPEN_CLEANUP(expr)                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_ALREADY_OPEN;                       \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_NOT_OPEN(expr)                                   \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_NOT_OPEN;                           \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_NOT_OPEN_CLEANUP(expr)                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_NOT_OPEN;                           \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_NOT_SUPPORTED(expr)                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;                      \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_DEVICE_NOT_SUPPORTED_CLEANUP(expr)                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;                      \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_HOSTNAME_INVALID(expr)                                  \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_HOSTNAME_INVALID;                          \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_HOSTNAME_INVALID_CLEANUP(expr)                          \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_HOSTNAME_INVALID;                          \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_PARAMETERS(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_PARAMETERS;                                \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_PARAMETERS_CLEANUP(expr)                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_PARAMETERS;                                \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_DRIVER_PATH_REQUIRED(expr)                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DRIVER_PATH_REQUIRED;                      \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_DRIVER_PATH_REQUIRED_CLEANUP(expr)                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_DRIVER_PATH_REQUIRED;                      \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_SYSTEM_ERROR(expr)                                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_SYSTEM_ERROR;                              \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_SYSTEM_ERROR_CLEANUP(expr)                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_SYSTEM_ERROR;                              \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_LIBRARY_ERROR(expr)                                     \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_LIBRARY_ERROR;                             \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_LIBRARY_ERROR_CLEANUP(expr)                             \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_LIBRARY_ERROR;                             \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_ERR_INTERNAL_ERROR(expr)                                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_INTERNAL_ERROR;                            \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_INTERNAL_ERROR_CLEANUP(expr)                            \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      dev->errnum = IPMI_ERR_INTERNAL_ERROR;                            \
      __UDM_SYSLOG;                                                     \
      __UDM_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define UDM_BAD_COMPLETION_CODE_TO_UDM_ERRNUM(__dev, __rs)                                                       \
do {                                                                                                               \
  if (ipmi_check_completion_code((__rs), IPMI_COMP_CODE_NODE_BUSY) == 1                                            \
      || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_OUT_OF_SPACE) == 1                                      \
      || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_SDR_UPDATE_MODE) == 1                                   \
      || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_FIRMWARE_UPDATE_MODE) == 1                              \
      || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_BMC_INIT_MODE) == 1)                                    \
    (__dev)->errnum = IPMI_ERR_BMC_BUSY;                                                                           \
  else if (ipmi_check_completion_code((__rs), IPMI_COMP_CODE_COMMAND_INVALID) == 1                                 \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_COMMAND_INVALID_FOR_LUN) == 1)                     \
    (__dev)->errnum = IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND;                                                \
  else if (ipmi_check_completion_code((__rs), IPMI_COMP_CODE_REQUEST_DATA_TRUNCATED) == 1                          \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID) == 1                  \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_REQUEST_DATA_LENGTH_LIMIT_EXCEEDED) == 1           \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE) == 1                       \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1    \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1                   \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE) == 1    \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_DESTINATION_UNAVAILABLE) == 1                      \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED) == 1              \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_REQUEST_PARAMETER_ILLEGAL) == 1)                   \
    (__dev)->errnum = IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID;                                           \
  else if (ipmi_check_completion_code((__rs), IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL) == 1)                   \
    (__dev)->errnum = IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT;                                                       \
  else                                                                                                             \
    (__dev)->errnum = IPMI_ERR_BAD_COMPLETION_CODE;                                                                \
} while (0)

/* Note: dev->errnum set in call to ipmi_cmd() - don't call wrapper */
#define UDM_ERR_IPMI_CMD_CLEANUP(__dev, __lun, __netfn, __rq, __rs)                                        \
do {                                                                                                       \
  int8_t __rv;                                                                                             \
  if (ipmi_cmd ((__dev),                                                                                   \
                (__lun),                                                                                   \
                (__netfn),                                                                                 \
                (__rq),                                                                                    \
                (__rs)) < 0)                                                                               \
    goto cleanup;                                                                                          \
  UDM_ERR_CLEANUP (!((__rv = ipmi_check_completion_code_success ((__rs))) < 0));                           \
  if (!__rv)                                                                                               \
    {                                                                                                      \
      UDM_BAD_COMPLETION_CODE_TO_UDM_ERRNUM((__dev), (__rs));                                              \
      __UDM_TRACE_ERRMSG_CLEANUP(__dev, __rs);                                                             \
    }                                                                                                      \
} while (0)

#define ERR_UDM(expr)                                                   \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_UDM_SYSLOG;                                                 \
      __ERR_UDM_TRACE;                                                  \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define ERR_UDM_CLEANUP(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERR_UDM_SYSLOG;                                                 \
      __ERR_UDM_TRACE;                                                  \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __KCS_ERRNUM_TO_UDM_ERRNUM                                \
do {                                                              \
  int32_t __errnum = ipmi_kcs_ctx_errnum(dev->io.inband.kcs_ctx); \
  if (__errnum == IPMI_KCS_CTX_ERR_SUCCESS)                       \
    dev->errnum = IPMI_ERR_SUCCESS;                               \
  else if (__errnum == IPMI_KCS_CTX_ERR_OUT_OF_MEMORY)            \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;                         \
  else if (__errnum == IPMI_KCS_CTX_ERR_PERMISSION)               \
    dev->errnum = IPMI_ERR_PERMISSION;                            \
  else if (__errnum == IPMI_KCS_CTX_ERR_PARAMETERS)               \
    dev->errnum = IPMI_ERR_LIBRARY_ERROR;                         \
  else if (__errnum == IPMI_KCS_CTX_ERR_DEVICE_NOT_FOUND)         \
    dev->errnum = IPMI_ERR_DEVICE_NOT_FOUND;                      \
  else if (__errnum == IPMI_KCS_CTX_ERR_SYSTEM_ERROR)             \
    dev->errnum = IPMI_ERR_SYSTEM_ERROR;                          \
  else                                                            \
    dev->errnum = IPMI_ERR_INTERNAL_ERROR;                        \
} while (0)

#define UDM_ERR_KCS(expr)                                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __KCS_ERRNUM_TO_UDM_ERRNUM;                                       \
      __UDM_KCS_SYSLOG;                                                 \
      __UDM_KCS_TRACE;                                                  \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_KCS_CLEANUP(expr)                                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __KCS_ERRNUM_TO_UDM_ERRNUM;                                       \
      __UDM_KCS_SYSLOG;                                                 \
      __UDM_KCS_TRACE;                                                  \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __SSIF_ERRNUM_TO_UDM_ERRNUM                                 \
do {                                                                \
  int32_t __errnum = ipmi_ssif_ctx_errnum(dev->io.inband.ssif_ctx); \
  if (__errnum == IPMI_SSIF_CTX_ERR_SUCCESS)                        \
    dev->errnum = IPMI_ERR_SUCCESS;                                 \
  else if (__errnum == IPMI_SSIF_CTX_ERR_OUT_OF_MEMORY)             \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;                           \
  else if (__errnum == IPMI_SSIF_CTX_ERR_PERMISSION)                \
    dev->errnum = IPMI_ERR_PERMISSION;                              \
  else if (__errnum == IPMI_SSIF_CTX_ERR_PARAMETERS)                \
    dev->errnum = IPMI_ERR_LIBRARY_ERROR;                           \
  else if (__errnum == IPMI_SSIF_CTX_ERR_DEVICE_NOT_FOUND)          \
    dev->errnum = IPMI_ERR_DEVICE_NOT_FOUND;                        \
  else                                                              \
    dev->errnum = IPMI_ERR_INTERNAL_ERROR;                          \
} while (0)

#define UDM_ERR_SSIF(expr)                                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __SSIF_ERRNUM_TO_UDM_ERRNUM;                                      \
      __UDM_SSIF_SYSLOG;                                                \
      __UDM_SSIF_TRACE;                                                 \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_SSIF_CLEANUP(expr)                                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __SSIF_ERRNUM_TO_UDM_ERRNUM;                                      \
      __UDM_SSIF_SYSLOG;                                                \
      __UDM_SSIF_TRACE;                                                 \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __OPENIPMI_ERRNUM_TO_UDM_ERRNUM                                     \
do {                                                                        \
  int32_t __errnum = ipmi_openipmi_ctx_errnum(dev->io.inband.openipmi_ctx); \
  if (__errnum == IPMI_OPENIPMI_CTX_ERR_SUCCESS)                            \
    dev->errnum = IPMI_ERR_SUCCESS;                                         \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_OUT_OF_MEMORY)                 \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;                                   \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_PERMISSION)                    \
    dev->errnum = IPMI_ERR_PERMISSION;                                      \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_DEVICE_NOT_FOUND)              \
    dev->errnum = IPMI_ERR_DEVICE_NOT_FOUND;                                \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_PARAMETERS)                    \
    dev->errnum = IPMI_ERR_LIBRARY_ERROR;                                   \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_SYSTEM_ERROR)                  \
    dev->errnum = IPMI_ERR_SYSTEM_ERROR;                                    \
  else                                                                      \
    dev->errnum = IPMI_ERR_INTERNAL_ERROR;                                  \
} while (0)

#define UDM_ERR_OPENIPMI(expr)                                          \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __OPENIPMI_ERRNUM_TO_UDM_ERRNUM;                                  \
      __UDM_OPENIPMI_SYSLOG;                                            \
      __UDM_OPENIPMI_TRACE;                                             \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_OPENIPMI_CLEANUP(expr)                                  \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __OPENIPMI_ERRNUM_TO_UDM_ERRNUM;                                  \
      __UDM_OPENIPMI_SYSLOG;                                            \
      __UDM_OPENIPMI_TRACE;                                             \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __LOCATE_ERRNUM_TO_UDM_ERRNUM(__errnum)                         \
do {                                                                    \
  if ((__errnum) == IPMI_LOCATE_ERR_SUCCESS)                            \
    dev->errnum = IPMI_ERR_SUCCESS;                                     \
  else if ((__errnum) == IPMI_LOCATE_ERR_OUT_OF_MEMORY)                 \
    dev->errnum = IPMI_ERR_OUT_OF_MEMORY;                               \
  else if ((__errnum) == IPMI_LOCATE_ERR_PERMISSION)                    \
    dev->errnum = IPMI_ERR_PERMISSION;                                  \
  else if ((__errnum) == IPMI_LOCATE_ERR_PARAMETERS)                    \
    dev->errnum = IPMI_ERR_LIBRARY_ERROR;                               \
  else if ((__errnum) == IPMI_LOCATE_ERR_SYSTEM_ERROR)                  \
    dev->errnum = IPMI_ERR_SYSTEM_ERROR;                                \
  else                                                                  \
    dev->errnum = IPMI_ERR_INTERNAL_ERROR;                              \
} while (0)

#define UDM_ERR_LOCATE(expr)                                            \
do {                                                                    \
  int __locate_errnum;                                                  \
  if ((__locate_errnum = (expr)))                                       \
    {                                                                   \
      __LOCATE_ERRNUM_TO_UDM_ERRNUM(__locate_errnum);                   \
      __UDM_LOCATE_SYSLOG;                                              \
      __UDM_LOCATE_TRACE;                                               \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define UDM_ERR_LOCATE_CLEANUP(expr)                                    \
do {                                                                    \
  int __locate_errnum;                                                  \
  if ((__locate_errnum = (expr)))                                       \
    {                                                                   \
      __LOCATE_ERRNUM_TO_UDM_ERRNUM(__locate_errnum);                   \
      __UDM_LOCATE_SYSLOG;                                              \
      __UDM_LOCATE_TRACE;                                               \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#ifdef __cplusplus
}
#endif

#endif /* udm-err-wrappers.h */

