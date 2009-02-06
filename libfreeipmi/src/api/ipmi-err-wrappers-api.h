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
#include "freeipmi/util/ipmi-error-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#if defined (IPMI_TRACE)
#define __API_CTX_TRACE                                                      \
do {                                                                         \
  int __ctxerrnum = ctx->errnum;                                             \
  char *__ctxerrstr = ipmi_ctx_strerror(__ctxerrnum);                        \
  __TRACE_CTX;                                                               \
} while (0)

#define __API_TRACE_COMP_CODE_ERRMSG(___ctx, ___rs)                          \
do {                                                                         \
  int __ctxerrnum = 0;                                                       \
  char __ctxerrstr[IPMI_ERR_STR_MAX_LEN];                                    \
  memset(__ctxerrstr, '\0', IPMI_ERR_STR_MAX_LEN);                           \
  ipmi_completion_code_strerror_cmd_r ((___rs),                              \
		                       (___ctx)->net_fn,                     \
		                       __ctxerrstr,                          \
		                       IPMI_ERR_STR_MAX_LEN);                \
  __TRACE_CTX;                                                               \
} while (0) 
#else
#define __API_CTX_TRACE
#define __API_TRACE_COMP_CODE_ERRMSG(__ctx, __rs)
#endif /* IPMI_TRACE */

#define API_SET_ERRNUM(__errnum)                                         \
do {                                                                     \
  ctx->errnum = (__errnum);                                              \
  __API_CTX_TRACE;                                                       \
} while (0)   

#define API_ERRNO_TO_API_ERRNUM(__ctx, __errno)                          \
do {                                                                     \
  ipmi_set_api_errnum_by_errno(__ctx, __errno);                          \
  __TRACE_ERRNO;                                                         \
} while (0)   

#define API_FIID_OBJECT_ERROR_TO_API_ERRNUM(__ctx, __obj)                \
do {                                                                     \
  ipmi_set_api_errnum_by_fiid_object((__ctx), (__obj));                  \
  __MSG_TRACE(fiid_obj_errormsg((__obj)), fiid_obj_errnum((__obj)));     \
} while (0)   

#define API_BAD_RESPONSE_TO_API_ERRNUM(__ctx, __obj_rs)                  \
do {                                                                     \
  ipmi_set_api_errnum_by_bad_response(__ctx, __obj_rs);                  \
  __API_CTX_TRACE;                                                       \
} while (0)

#define API_KCS_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)                    \
do {                                                                     \
  ipmi_set_api_errnum_by_kcs_errnum(__ctx, __errnum);                    \
  __MSG_TRACE(ipmi_kcs_ctx_strerror(__errnum), __errnum);                \
} while (0)   

#define API_SSIF_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)                   \
do {                                                                     \
  ipmi_set_api_errnum_by_ssif_errnum(__ctx, __errnum);                   \
  __MSG_TRACE(ipmi_ssif_ctx_strerror(__errnum), __errnum);               \
} while (0)   

#define API_OPENIPMI_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)               \
do {                                                                     \
  ipmi_set_api_errnum_by_openipmi_errnum(__ctx, __errnum);               \
  __MSG_TRACE(ipmi_openipmi_ctx_strerror(__errnum), __errnum);           \
} while (0)   

#define API_SUNBMC_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)                 \
do {                                                                     \
  ipmi_set_api_errnum_by_sunbmc_errnum(__ctx, __errnum);                 \
  __MSG_TRACE(ipmi_sunbmc_ctx_strerror(__errnum), __errnum);             \
} while (0)   

#define API_LOCATE_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)                 \
do {                                                                     \
  ipmi_set_api_errnum_by_locate_errnum(__ctx, __errnum);                 \
  __MSG_TRACE(ipmi_locate_strerror(__errnum), __errnum);                 \
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
  if ((__rv = ipmi_check_completion_code_success ((__rs))) < 0)                                            \
    {                                                                                                      \
      API_ERRNO_TO_API_ERRNUM((__ctx), errno);                                                             \
      goto cleanup;                                                                                        \
    }                                                                                                      \
  if (!__rv)                                                                                               \
    {                                                                                                      \
      ipmi_set_api_errnum_by_bad_response((__ctx), (__rs));                                                \
      __API_TRACE_COMP_CODE_ERRMSG(__ctx, __rs);                                                           \
      goto cleanup;                                                                                        \
    }                                                                                                      \
} while (0)

/* Note: ctx->errnum set in call to ipmi_cmd_ipmb() - don't call wrapper */
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
  if ((__rv = ipmi_check_completion_code_success ((__rs))) < 0)                                            \
    {                                                                                                      \
      API_ERRNO_TO_API_ERRNUM((__ctx), errno);                                                             \
      goto cleanup;                                                                                        \
    }                                                                                                      \
  if (!__rv)                                                                                               \
    {                                                                                                      \
      ipmi_set_api_errnum_by_bad_response((__ctx), (__rs));                                                \
      __API_TRACE_COMP_CODE_ERRMSG(__ctx, __rs);                                                           \
      goto cleanup;                                                                                        \
    }                                                                                                      \
} while (0)

void ipmi_set_api_errnum_by_errno(ipmi_ctx_t ctx, int __errno);

void ipmi_set_api_errnum_by_fiid_object(ipmi_ctx_t ctx, fiid_obj_t obj);

void ipmi_set_api_errnum_by_bad_response(ipmi_ctx_t ctx, fiid_obj_t obj_cmd_rs);

void ipmi_set_api_errnum_by_locate_errnum(ipmi_ctx_t ctx, int locate_errnum);

void ipmi_set_api_errnum_by_kcs_errnum(ipmi_ctx_t ctx, int kcs_errnum);

void ipmi_set_api_errnum_by_ssif_errnum(ipmi_ctx_t ctx, int ssif_errnum);

void ipmi_set_api_errnum_by_openipmi_errnum(ipmi_ctx_t ctx, int openipmi_errnum);

void ipmi_set_api_errnum_by_sunbmc_errnum(ipmi_ctx_t ctx, int sunbmc_errnum);

int api_fiid_obj_packet_valid(ipmi_ctx_t ctx, fiid_obj_t obj);

int api_fiid_obj_template_compare (ipmi_ctx_t ctx, fiid_obj_t obj, fiid_template_t tmpl);

int api_fiid_obj_get(ipmi_ctx_t ctx, fiid_obj_t obj, char *field, uint64_t *val);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-err-wrappers-api.h */

