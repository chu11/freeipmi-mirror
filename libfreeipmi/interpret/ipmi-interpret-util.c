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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/interpret/ipmi-interpret.h"

#include "ipmi-interpret-defs.h"
#include "ipmi-interpret-trace.h"
#include "ipmi-interpret-util.h"

#include "freeipmi-portability.h"

void
interpret_set_interpret_errnum_by_errno (ipmi_interpret_ctx_t ctx, int __errno)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    return;

  if (__errno == 0)
    ctx->errnum = IPMI_INTERPRET_ERR_SUCCESS;
  else if (__errno == ENOMEM)
    ctx->errnum = IPMI_INTERPRET_ERR_OUT_OF_MEMORY;
  else
    ctx->errnum = IPMI_INTERPRET_ERR_INTERNAL_ERROR;
}

void
interpret_set_interpret_errnum_by_sel_ctx (ipmi_interpret_ctx_t ctx, ipmi_sel_ctx_t sel_ctx)
{
  if (!ctx || ctx->magic != IPMI_INTERPRET_CTX_MAGIC)
    return;

  if (ipmi_sel_ctx_errnum (sel_ctx) == IPMI_SEL_ERR_INVALID_SEL_ENTRY)
    ctx->errnum = IPMI_INTERPRET_ERR_INVALID_SEL_RECORD;
  else if (ipmi_sel_ctx_errnum (sel_ctx) == IPMI_SEL_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_INTERPRET_ERR_OUT_OF_MEMORY;
  else if (ipmi_sel_ctx_errnum (sel_ctx) == IPMI_SEL_ERR_SYSTEM_ERROR)
    ctx->errnum = IPMI_INTERPRET_ERR_SYSTEM_ERROR;
  else
    ctx->errnum = IPMI_INTERPRET_ERR_INTERNAL_ERROR;
}
