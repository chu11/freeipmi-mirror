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
#include <assert.h>
#include <errno.h>

#include "freeipmi/locate/ipmi-locate.h"
#include "freeipmi/fiid/fiid.h"

#include "ipmi-locate-defs.h"
#include "ipmi-locate-trace.h"
#include "ipmi-locate-util.h"

#include "freeipmi-portability.h"

void
locate_set_locate_errnum_by_errno (ipmi_locate_ctx_t ctx, int __errno)
{
  assert (ctx && ctx->magic == IPMI_LOCATE_CTX_MAGIC);

  switch (__errno)
    {
    case 0:
      ctx->errnum = IPMI_LOCATE_ERR_SUCCESS;
      break;
    case EPERM:
      ctx->errnum = IPMI_LOCATE_ERR_PERMISSION;
      break;
    case EACCES:
      ctx->errnum = IPMI_LOCATE_ERR_PERMISSION;
      break;
    case ENOMEM:
      ctx->errnum = IPMI_LOCATE_ERR_OUT_OF_MEMORY;
      break;
    case EINVAL:
      ctx->errnum = IPMI_LOCATE_ERR_INTERNAL_ERROR;
      break;
    default:
      ctx->errnum = IPMI_LOCATE_ERR_SYSTEM_ERROR;
    }
}

void
locate_set_locate_errnum_by_fiid_object (ipmi_locate_ctx_t ctx, fiid_obj_t obj)
{
  assert (ctx && ctx->magic == IPMI_LOCATE_CTX_MAGIC);

  switch (fiid_obj_errnum (obj))
    {
    case FIID_ERR_SUCCESS:
      ctx->errnum = IPMI_LOCATE_ERR_SUCCESS;
      break;
    case FIID_ERR_OUT_OF_MEMORY:
      ctx->errnum = IPMI_LOCATE_ERR_OUT_OF_MEMORY;
      break;
    case FIID_ERR_DATA_NOT_AVAILABLE:
      ctx->errnum = IPMI_LOCATE_ERR_SYSTEM_ERROR;
      break;
    case FIID_ERR_FIELD_NOT_FOUND:
    case FIID_ERR_DATA_NOT_BYTE_ALIGNED:
    case FIID_ERR_REQUIRED_FIELD_MISSING:
    case FIID_ERR_FIXED_LENGTH_FIELD_INVALID:
    case FIID_ERR_NOT_IDENTICAL:
      ctx->errnum = IPMI_LOCATE_ERR_PARAMETERS;
      break;
    default:
      ctx->errnum = IPMI_LOCATE_ERR_INTERNAL_ERROR;
    }
}
