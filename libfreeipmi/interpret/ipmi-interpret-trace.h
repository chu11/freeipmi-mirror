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

#ifndef IPMI_INTERPRET_TRACE_H
#define IPMI_INTERPRET_TRACE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "libcommon/ipmi-trace.h"

#define INTERPRET_SET_ERRNUM(__ctx, __errnum)                                               \
  do {                                                                                      \
    (__ctx)->errnum = (__errnum);                                                           \
    TRACE_MSG_OUT (ipmi_interpret_ctx_errormsg ((__ctx)), (__errnum));                      \
  } while (0)

#define INTERPRET_ERRNO_TO_INTERPRET_ERRNUM(__ctx, __errno)                                 \
  do {                                                                                      \
    interpret_set_interpret_errnum_by_errno ((__ctx), (__errno));                           \
    TRACE_ERRNO_OUT ((__errno));                                                            \
  } while (0)

#define INTERPRET_SEL_CTX_ERROR_TO_INTERPRET_ERRNUM(__ctx, __sel_ctx)                       \
  do {                                                                                      \
    interpret_set_interpret_errnum_by_sel_ctx ((__ctx), (__sel_ctx));                       \
    TRACE_MSG_OUT (ipmi_sel_ctx_errormsg ((__sel_ctx)), ipmi_sel_ctx_errnum ((__sel_ctx))); \
  } while (0)

#endif /* IPMI_INTERPRET_TRACE_H */
