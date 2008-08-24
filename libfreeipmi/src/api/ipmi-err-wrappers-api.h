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

#ifndef _IPMI_ERR_WRAPPERS_API_H
#define	_IPMI_ERR_WRAPPERS_API_H

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

#include "freeipmi/api/ipmi-api.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-error-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "libcommon/ipmi-err-wrappers.h"

#if defined (IPMI_TRACE)
#define __API_TRACE                                                          \
do {                                                                         \
  int __ctxerrnum = ctx->errnum;                                             \
  char *__ctxerrstr = ipmi_ctx_strerror(__ctxerrnum);                        \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)

#define __API_TRACE_ERRMSG_CLEANUP(___ctx, ___rs)                            \
do {                                                                         \
  int __ctxerrnum = 0;                                                       \
  char __ctxerrstr[IPMI_ERR_STR_MAX_LEN];                                    \
  memset(__ctxerrstr, '\0', IPMI_ERR_STR_MAX_LEN);                           \
  ipmi_completion_code_strerror_cmd_r ((___rs),                              \
		                       (___ctx)->net_fn,                     \
		                       __ctxerrstr,                          \
		                       IPMI_ERR_STR_MAX_LEN);                \
  __TRACE_CTX_OUTPUT;                                                        \
  goto cleanup;                                                              \
} while (0) 

#define __API_KCS_TRACE                                                      \
do {                                                                         \
  int __ctxerrnum = ipmi_kcs_ctx_errnum(ctx->io.inband.kcs_ctx);             \
  char *__ctxerrstr = ipmi_kcs_ctx_strerror(__ctxerrnum);                    \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)

#define __API_SSIF_TRACE                                                     \
do {                                                                         \
  int __ctxerrnum = ipmi_ssif_ctx_errnum(ctx->io.inband.ssif_ctx);           \
  char *__ctxerrstr = ipmi_ssif_ctx_strerror(__ctxerrnum);                   \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)

#define __API_OPENIPMI_TRACE                                                 \
do {                                                                         \
  int __ctxerrnum = ipmi_openipmi_ctx_errnum(ctx->io.inband.openipmi_ctx);   \
  char *__ctxerrstr = ipmi_openipmi_ctx_strerror(__ctxerrnum);               \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)

#define __API_SUNBMC_TRACE                                                   \
do {                                                                         \
  int __ctxerrnum = ipmi_sunbmc_ctx_errnum(ctx->io.inband.sunbmc_ctx);       \
  char *__ctxerrstr = ipmi_sunbmc_ctx_strerror(__ctxerrnum);                 \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)

#define __API_LOCATE_TRACE                                                   \
do {                                                                         \
  int __ctxerrnum = __locate_errnum;                                         \
  char *__ctxerrstr = ipmi_locate_strerror(__ctxerrnum);                     \
  __TRACE_CTX_OUTPUT;                                                        \
} while (0)
#else
#define __API_TRACE
#define __API_TRACE_ERRMSG_CLEANUP(__ctx, __rs)                              \
do {                                                                         \
  goto cleanup;                                                              \
} while (0) 
#define __API_KCS_TRACE
#define __API_SSIF_TRACE
#define __API_OPENIPMI_TRACE
#define __API_SUNBMC_TRACE
#define __API_LOCATE_TRACE
#endif /* IPMI_TRACE */

#define __ERRNO_TO_API_ERRNUM                      \
do {                                               \
  if (errno == 0)                                  \
    ctx->errnum = IPMI_ERR_SUCCESS;                \
  else if (errno == ENOMEM)                        \
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;          \
  else if (errno == ENODEV)                        \
    ctx->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;   \
  else if (errno == EINVAL)                        \
    ctx->errnum = IPMI_ERR_LIBRARY_ERROR;          \
  else                                             \
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;         \
} while (0)

#define API_ERR_CTX_CHECK(expr)                                         \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      errno = EINVAL;                                                   \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_SET_ERRNUM(__errnum)                                    \
do {                                                                    \
  ctx->errnum = (__errnum);                                             \
  __API_TRACE;                                                          \
} while (0)   

#define API_ERR_SET_ERRNUM_CLEANUP(__errnum)                            \
do {                                                                    \
  ctx->errnum = (__errnum);                                             \
  __API_TRACE;                                                          \
  goto cleanup;                                                         \
} while (0)   

#define API_ERR_LOG(expr)                                               \
do {                                                                    \
  __API_TRACE;                                                          \
} while (0)   

#define API_ERR_LOG_CLEANUP(expr)                                       \
do {                                                                    \
  __API_TRACE;                                                          \
  goto cleanup;                                                         \
} while (0)   

#define API_ERR(expr)                                                   \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERRNO_TO_API_ERRNUM;                                            \
      __ERR_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_CLEANUP(expr)                                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __ERRNO_TO_API_ERRNUM;                                            \
      __ERR_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_DEVICE_ALREADY_OPEN(expr)                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_DEVICE_ALREADY_OPEN;                       \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_DEVICE_ALREADY_OPEN_CLEANUP(expr)                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_DEVICE_ALREADY_OPEN;                       \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_DEVICE_NOT_OPEN(expr)                                   \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_DEVICE_NOT_OPEN;                           \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_DEVICE_NOT_OPEN_CLEANUP(expr)                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_DEVICE_NOT_OPEN;                           \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_DEVICE_NOT_SUPPORTED(expr)                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;                      \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_DEVICE_NOT_SUPPORTED_CLEANUP(expr)                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;                      \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE(expr)            \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE;    \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE_CLEANUP(expr)    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE;    \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_HOSTNAME_INVALID(expr)                                  \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_HOSTNAME_INVALID;                          \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_HOSTNAME_INVALID_CLEANUP(expr)                          \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_HOSTNAME_INVALID;                          \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_PARAMETERS(expr)                                        \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_PARAMETERS;                                \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_PARAMETERS_CLEANUP(expr)                                \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_PARAMETERS;                                \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_MESSAGE_TIMEOUT(expr)                                   \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_MESSAGE_TIMEOUT;                           \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_MESSAGE_TIMEOUT_CLEANUP(expr)                           \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_MESSAGE_TIMEOUT;                           \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_DRIVER_PATH_REQUIRED(expr)                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_DRIVER_PATH_REQUIRED;                      \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_DRIVER_PATH_REQUIRED_CLEANUP(expr)                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_DRIVER_PATH_REQUIRED;                      \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_SYSTEM_ERROR(expr)                                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_SYSTEM_ERROR;                              \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_SYSTEM_ERROR_CLEANUP(expr)                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_SYSTEM_ERROR;                              \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_LIBRARY_ERROR(expr)                                     \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_LIBRARY_ERROR;                             \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_LIBRARY_ERROR_CLEANUP(expr)                             \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_LIBRARY_ERROR;                             \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_ERR_INTERNAL_ERROR(expr)                                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_INTERNAL_ERROR;                            \
      __API_TRACE;                                                      \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_INTERNAL_ERROR_CLEANUP(expr)                            \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      ctx->errnum = IPMI_ERR_INTERNAL_ERROR;                            \
      __API_TRACE;                                                      \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define API_BAD_COMPLETION_CODE_TO_API_ERRNUM(__ctx, __rs)                                                       \
do {                                                                                                               \
  if (ipmi_check_completion_code((__rs), IPMI_COMP_CODE_NODE_BUSY) == 1                                            \
      || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_OUT_OF_SPACE) == 1                                      \
      || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_SDR_UPDATE_MODE) == 1                                   \
      || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_FIRMWARE_UPDATE_MODE) == 1                              \
      || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_BMC_INIT_MODE) == 1)                                    \
    (__ctx)->errnum = IPMI_ERR_BMC_BUSY;                                                                           \
  else if (ipmi_check_completion_code((__rs), IPMI_COMP_CODE_COMMAND_INVALID) == 1                                 \
	   || ipmi_check_completion_code((__rs), IPMI_COMP_CODE_COMMAND_INVALID_FOR_LUN) == 1)                     \
    (__ctx)->errnum = IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND;                                                \
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
    (__ctx)->errnum = IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID;                                           \
  else if (ipmi_check_completion_code((__rs), IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL) == 1)                   \
    (__ctx)->errnum = IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT;                                                       \
  else                                                                                                             \
    (__ctx)->errnum = IPMI_ERR_BAD_COMPLETION_CODE;                                                                \
} while (0)

/* Note: ctx->errnum set in call to ipmi_cmd() - don't call wrapper */
#define API_ERR_IPMI_CMD_CLEANUP(__ctx, __lun, __netfn, __rq, __rs)                                        \
do {                                                                                                       \
  int8_t __rv;                                                                                             \
  if (ipmi_cmd ((__ctx),                                                                                   \
                (__lun),                                                                                   \
                (__netfn),                                                                                 \
                (__rq),                                                                                    \
                (__rs)) < 0)                                                                               \
    goto cleanup;                                                                                          \
  API_ERR_CLEANUP (!((__rv = ipmi_check_completion_code_success ((__rs))) < 0));                           \
  if (!__rv)                                                                                               \
    {                                                                                                      \
      API_BAD_COMPLETION_CODE_TO_API_ERRNUM((__ctx), (__rs));                                              \
      __API_TRACE_ERRMSG_CLEANUP(__ctx, __rs);                                                             \
    }                                                                                                      \
} while (0)

/* Note: ctx->errnum set in call to ipmi_cmd() - don't call wrapper */
#define API_ERR_IPMI_CMD_IPMB_CLEANUP(__ctx, __slave_address, __lun, __netfn, __rq, __rs)                  \
do {                                                                                                       \
  int8_t __rv;                                                                                             \
  if (ipmi_cmd_ipmb ((__ctx),                                                                              \
                     (__slave_address),                                                                    \
                     (__lun),                                                                              \
                     (__netfn),                                                                            \
                     (__rq),                                                                               \
                     (__rs)) < 0)                                                                          \
    goto cleanup;                                                                                          \
  API_ERR_CLEANUP (!((__rv = ipmi_check_completion_code_success ((__rs))) < 0));                           \
  if (!__rv)                                                                                               \
    {                                                                                                      \
      API_BAD_COMPLETION_CODE_TO_API_ERRNUM((__ctx), (__rs));                                              \
      __API_TRACE_ERRMSG_CLEANUP(__ctx, __rs);                                                             \
    }                                                                                                      \
} while (0)

#define __KCS_ERRNUM_TO_API_ERRNUM                                \
do {                                                              \
  int32_t __errnum = ipmi_kcs_ctx_errnum(ctx->io.inband.kcs_ctx); \
  if (__errnum == IPMI_KCS_CTX_ERR_SUCCESS)                       \
    ctx->errnum = IPMI_ERR_SUCCESS;                               \
  else if (__errnum == IPMI_KCS_CTX_ERR_OUT_OF_MEMORY)            \
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;                         \
  else if (__errnum == IPMI_KCS_CTX_ERR_PERMISSION)               \
    ctx->errnum = IPMI_ERR_PERMISSION;                            \
  else if (__errnum == IPMI_KCS_CTX_ERR_PARAMETERS)               \
    ctx->errnum = IPMI_ERR_LIBRARY_ERROR;                         \
  else if (__errnum == IPMI_KCS_CTX_ERR_DEVICE_NOT_FOUND)         \
    ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;                      \
  else if (__errnum == IPMI_KCS_CTX_ERR_DRIVER_TIMEOUT)           \
    ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;                        \
  else if (__errnum == IPMI_KCS_CTX_ERR_BUSY)                     \
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;                          \
  else if (__errnum == IPMI_KCS_CTX_ERR_SYSTEM_ERROR)             \
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;                          \
  else                                                            \
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;                        \
} while (0)

#define API_ERR_KCS(expr)                                               \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __KCS_ERRNUM_TO_API_ERRNUM;                                       \
      __API_KCS_TRACE;                                                  \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_KCS_CLEANUP(expr)                                       \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __KCS_ERRNUM_TO_API_ERRNUM;                                       \
      __API_KCS_TRACE;                                                  \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __SSIF_ERRNUM_TO_API_ERRNUM                                 \
do {                                                                \
  int32_t __errnum = ipmi_ssif_ctx_errnum(ctx->io.inband.ssif_ctx); \
  if (__errnum == IPMI_SSIF_CTX_ERR_SUCCESS)                        \
    ctx->errnum = IPMI_ERR_SUCCESS;                                 \
  else if (__errnum == IPMI_SSIF_CTX_ERR_OUT_OF_MEMORY)             \
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;                           \
  else if (__errnum == IPMI_SSIF_CTX_ERR_PERMISSION)                \
    ctx->errnum = IPMI_ERR_PERMISSION;                              \
  else if (__errnum == IPMI_SSIF_CTX_ERR_PARAMETERS)                \
    ctx->errnum = IPMI_ERR_LIBRARY_ERROR;                           \
  else if (__errnum == IPMI_SSIF_CTX_ERR_DEVICE_NOT_FOUND)          \
    ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;                        \
  else if (__errnum == IPMI_SSIF_CTX_ERR_DRIVER_TIMEOUT)            \
    ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;                          \
  else if (__errnum == IPMI_SSIF_CTX_ERR_BUSY)                      \
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;                            \
  else if (__errnum == IPMI_SSIF_CTX_ERR_SYSTEM_ERROR)              \
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;                            \
  else                                                              \
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;                          \
} while (0)

#define API_ERR_SSIF(expr)                                              \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __SSIF_ERRNUM_TO_API_ERRNUM;                                      \
      __API_SSIF_TRACE;                                                 \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_SSIF_CLEANUP(expr)                                      \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __SSIF_ERRNUM_TO_API_ERRNUM;                                      \
      __API_SSIF_TRACE;                                                 \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __OPENIPMI_ERRNUM_TO_API_ERRNUM                                     \
do {                                                                        \
  int32_t __errnum = ipmi_openipmi_ctx_errnum(ctx->io.inband.openipmi_ctx); \
  if (__errnum == IPMI_OPENIPMI_CTX_ERR_SUCCESS)                            \
    ctx->errnum = IPMI_ERR_SUCCESS;                                         \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_OUT_OF_MEMORY)                 \
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;                                   \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_PERMISSION)                    \
    ctx->errnum = IPMI_ERR_PERMISSION;                                      \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_PARAMETERS)                    \
    ctx->errnum = IPMI_ERR_LIBRARY_ERROR;                                   \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_DEVICE_NOT_FOUND)              \
    ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;                                \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_DRIVER_TIMEOUT)                \
    ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;                                  \
  else if (__errnum == IPMI_OPENIPMI_CTX_ERR_SYSTEM_ERROR)                  \
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;                                    \
  else                                                                      \
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;                                  \
} while (0)

#define API_ERR_OPENIPMI(expr)                                          \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __OPENIPMI_ERRNUM_TO_API_ERRNUM;                                  \
      __API_OPENIPMI_TRACE;                                             \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_OPENIPMI_CLEANUP(expr)                                  \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __OPENIPMI_ERRNUM_TO_API_ERRNUM;                                  \
      __API_OPENIPMI_TRACE;                                             \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __SUNBMC_ERRNUM_TO_API_ERRNUM                                       \
do {                                                                        \
  int32_t __errnum = ipmi_sunbmc_ctx_errnum(ctx->io.inband.sunbmc_ctx);     \
  if (__errnum == IPMI_SUNBMC_CTX_ERR_SUCCESS)                              \
    ctx->errnum = IPMI_ERR_SUCCESS;                                         \
  else if (__errnum == IPMI_SUNBMC_CTX_ERR_OUT_OF_MEMORY)                   \
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;                                   \
  else if (__errnum == IPMI_SUNBMC_CTX_ERR_PERMISSION)                      \
    ctx->errnum = IPMI_ERR_PERMISSION;                                      \
  else if (__errnum == IPMI_SUNBMC_CTX_ERR_PARAMETERS)                      \
    ctx->errnum = IPMI_ERR_LIBRARY_ERROR;                                   \
  else if (__errnum == IPMI_SUNBMC_CTX_ERR_DEVICE_NOT_FOUND)                \
    ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;                                \
  else if (__errnum == IPMI_SUNBMC_CTX_ERR_DRIVER_TIMEOUT)                  \
    ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;                                  \
  else if (__errnum == IPMI_SUNBMC_CTX_ERR_SYSTEM_ERROR)                    \
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;                                    \
  else                                                                      \
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;                                  \
} while (0)

#define API_ERR_SUNBMC(expr)                                            \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __SUNBMC_ERRNUM_TO_API_ERRNUM;                                    \
      __API_SUNBMC_TRACE;                                               \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_SUNBMC_CLEANUP(expr)                                    \
do {                                                                    \
  if (!(expr))                                                          \
    {                                                                   \
      __SUNBMC_ERRNUM_TO_API_ERRNUM;                                    \
      __API_SUNBMC_TRACE;                                               \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#define __LOCATE_ERRNUM_TO_API_ERRNUM(__errnum)                         \
do {                                                                    \
  if ((__errnum) == IPMI_LOCATE_ERR_SUCCESS)                            \
    ctx->errnum = IPMI_ERR_SUCCESS;                                     \
  else if ((__errnum) == IPMI_LOCATE_ERR_OUT_OF_MEMORY)                 \
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;                               \
  else if ((__errnum) == IPMI_LOCATE_ERR_PERMISSION)                    \
    ctx->errnum = IPMI_ERR_PERMISSION;                                  \
  else if ((__errnum) == IPMI_LOCATE_ERR_PARAMETERS)                    \
    ctx->errnum = IPMI_ERR_LIBRARY_ERROR;                               \
  else if ((__errnum) == IPMI_LOCATE_ERR_SYSTEM_ERROR)                  \
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;                                \
  else                                                                  \
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;                              \
} while (0)

#define API_ERR_LOCATE(expr)                                            \
do {                                                                    \
  int __locate_errnum;                                                  \
  if ((__locate_errnum = (expr)))                                       \
    {                                                                   \
      __LOCATE_ERRNUM_TO_API_ERRNUM(__locate_errnum);                   \
      __API_LOCATE_TRACE;                                               \
      return (-1);                                                      \
    }                                                                   \
} while (0)

#define API_ERR_LOCATE_CLEANUP(expr)                                    \
do {                                                                    \
  int __locate_errnum;                                                  \
  if ((__locate_errnum = (expr)))                                       \
    {                                                                   \
      __LOCATE_ERRNUM_TO_API_ERRNUM(__locate_errnum);                   \
      __API_LOCATE_TRACE;                                               \
      goto cleanup;                                                     \
    }                                                                   \
} while (0)

#ifdef __cplusplus
}
#endif

#endif /* ipmi-err-wrappers-api.h */

