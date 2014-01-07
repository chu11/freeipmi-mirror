/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef IPMI_API_TRACE_H
#define IPMI_API_TRACE_H

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

#include "libcommon/ipmi-trace.h"

#include "ipmi-api-util.h"

#if defined (IPMI_TRACE)
#define API_BAD_RESPONSE_TRACE(___ctx, ___rs)                               \
  do {                                                                      \
    int __rserrnum = 0;                                                     \
    uint64_t __rsval = 0;                                                   \
    char __rserrstr[IPMI_ERR_STR_MAX_LEN];                                  \
    fiid_obj_get (___rs, "comp_code", &__rsval);                            \
    __rserrnum = __rsval;                                                   \
    memset (__rserrstr, '\0', IPMI_ERR_STR_MAX_LEN);                        \
    ipmi_completion_code_strerror_cmd_r ((___rs),                           \
                                         (___ctx)->target.net_fn,           \
                                         __rserrstr,                        \
                                         IPMI_ERR_STR_MAX_LEN);             \
    TRACE_MSG_OUT (__rserrstr, __rserrnum);                                 \
  } while (0)
#else
#define API_BAD_RESPONSE_TRACE(__ctx, __rs)
#endif /* IPMI_TRACE */

#define API_SET_ERRNUM(__ctx, __errnum)                                     \
  do {                                                                      \
    (__ctx)->errnum = (__errnum);                                           \
    TRACE_MSG_OUT (ipmi_ctx_errormsg ((__ctx)), __errnum);                  \
  } while (0)

#define API_ERRNO_TO_API_ERRNUM(__ctx, __errno)                             \
  do {                                                                      \
    api_set_api_errnum_by_errno ((__ctx), (__errno));                       \
    TRACE_ERRNO_OUT ((__errno));                                            \
  } while (0)

#define API_FIID_OBJECT_ERROR_TO_API_ERRNUM(__ctx, __obj)                   \
  do {                                                                      \
    api_set_api_errnum_by_fiid_object ((__ctx), (__obj));                   \
    TRACE_MSG_OUT (fiid_obj_errormsg ((__obj)), fiid_obj_errnum ((__obj))); \
  } while (0)

#define API_BAD_RESPONSE_TO_API_ERRNUM(__ctx, __obj_rs)                     \
  do {                                                                      \
    api_set_api_errnum_by_bad_response ((__ctx), (__obj_rs));               \
    API_BAD_RESPONSE_TRACE ((__ctx), (__obj_rs));                           \
  } while (0)

#define API_KCS_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)                       \
  do {                                                                      \
    api_set_api_errnum_by_kcs_errnum ((__ctx), (__errnum));                 \
    TRACE_MSG_OUT (ipmi_kcs_ctx_strerror ((__errnum)), (__errnum));         \
  } while (0)

#define API_SSIF_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)                      \
  do {                                                                      \
    api_set_api_errnum_by_ssif_errnum ((__ctx), (__errnum));                \
    TRACE_MSG_OUT (ipmi_ssif_ctx_strerror ((__errnum)), (__errnum));        \
  } while (0)

#define API_OPENIPMI_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)                  \
  do {                                                                      \
    api_set_api_errnum_by_openipmi_errnum ((__ctx), (__errnum));            \
    TRACE_MSG_OUT (ipmi_openipmi_ctx_strerror ((__errnum)), (__errnum));    \
  } while (0)

#define API_SUNBMC_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)                    \
  do {                                                                      \
    api_set_api_errnum_by_sunbmc_errnum ((__ctx), (__errnum));              \
    TRACE_MSG_OUT (ipmi_sunbmc_ctx_strerror ((__errnum)), (__errnum));      \
  } while (0)

#define API_INTELDCMI_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)                 \
  do {                                                                      \
    api_set_api_errnum_by_inteldcmi_errnum ((__ctx), (__errnum));           \
    TRACE_MSG_OUT (ipmi_inteldcmi_ctx_strerror ((__errnum)), (__errnum));   \
  } while (0)

#define API_LOCATE_ERRNUM_TO_API_ERRNUM(__ctx, __errnum)                    \
  do {                                                                      \
    api_set_api_errnum_by_locate_errnum ((__ctx), (__errnum));              \
    TRACE_MSG_OUT (ipmi_locate_ctx_strerror ((__errnum)), (__errnum));      \
  } while (0)

#endif /* IPMI_API_TRACE_H */

