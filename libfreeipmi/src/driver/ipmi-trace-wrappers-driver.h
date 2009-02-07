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

#ifndef _IPMI_ERR_WRAPPERS_DRIVER_H
#define	_IPMI_ERR_WRAPPERS_DRIVER_H

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

#define KCS_ERRNO_TO_KCS_ERRNUM(__ctx, __errno)                         \
  do {                                                                  \
    _set_kcs_ctx_errnum_by_errno(__ctx, __errno);                       \
    __ERRNO_TRACE(__errno);                                             \
  } while (0)   

#define KCS_ERRNUM_SET(__ctx, __errnum)                                 \
  do {                                                                  \
    (__ctx)->errnum = (__errnum);                                       \
    __MSG_TRACE(ipmi_kcs_ctx_errormsg((__ctx)), __errnum);              \
  } while (0)

#define SSIF_ERRNO_TO_SSIF_ERRNUM(__ctx, __errno)                       \
  do {                                                                  \
    _set_ssif_ctx_errnum_by_errno(__ctx, __errno);                      \
    __ERRNO_TRACE(__errno);                                             \
  } while (0)   

#define SSIF_ERRNUM_SET(__ctx, __errnum)                                \
  do {                                                                  \
    (__ctx)->errnum = (__errnum);                                       \
    __MSG_TRACE(ipmi_ssif_ctx_errormsg((__ctx)), __errnum);             \
  } while (0)

#define OPENIPMI_ERRNO_TO_OPENIPMI_ERRNUM(__ctx, __errno)               \
  do {                                                                  \
    _set_openipmi_ctx_errnum_by_errno(__ctx, __errno);                  \
    __ERRNO_TRACE(__errno);                                             \
  } while (0)   

#define OPENIPMI_ERRNUM_SET(__ctx, __errnum)                            \
  do {                                                                  \
    (__ctx)->errnum = (__errnum);                                       \
    __MSG_TRACE(ipmi_openipmi_ctx_errormsg((__ctx)), __errnum);         \
  } while (0)

#define SUNBMC_ERRNO_TO_SUNBMC_ERRNUM(__ctx, __errno)                   \
  do {                                                                  \
    _set_sunbmc_ctx_errnum_by_errno(__ctx, __errno);                    \
    __ERRNO_TRACE(__errno);                                             \
  } while (0)   

#define SUNBMC_ERRNUM_SET(__ctx, __errnum)                              \
  do {                                                                  \
    (__ctx)->errnum = (__errnum);                                       \
    __MSG_TRACE(ipmi_sunbmc_ctx_errormsg((__ctx)), __errnum);           \
  } while (0)

#ifdef __cplusplus
}
#endif

#endif /* ipmi-trace-wrappers-driver.h */

