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

#include "freeipmi/sdr/ipmi-sdr.h"
#include "freeipmi/fiid/fiid.h"

#include "ipmi-sdr-defs.h"
#include "ipmi-sdr-trace.h"
#include "ipmi-sdr-util.h"

#include "freeipmi-portability.h"

void
sdr_set_sdr_errnum_by_errno (ipmi_sdr_ctx_t ctx, int __errno)
{
  assert (ctx && ctx->magic == IPMI_SDR_CTX_MAGIC);

  switch (__errno)
    {
    case 0:
      ctx->errnum = IPMI_SDR_ERR_SUCCESS;
      break;
    case ENOSPC:
    case EMFILE:
    case ENFILE:
      ctx->errnum = IPMI_SDR_ERR_FILESYSTEM;
      break;
    case EPERM:
    case EACCES:
    case EISDIR:
    case EROFS:
      ctx->errnum = IPMI_SDR_ERR_PERMISSION;
      break;
    case ENOENT:
    case ENOTDIR:
      ctx->errnum = IPMI_SDR_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST;
      break;
    case ENAMETOOLONG:
    case ELOOP:
      ctx->errnum = IPMI_SDR_ERR_FILENAME_INVALID;
      break;
    case ENOMEM:
      ctx->errnum = IPMI_SDR_ERR_OUT_OF_MEMORY;
      break;
    case EINVAL:
      ctx->errnum = IPMI_SDR_ERR_INTERNAL_ERROR;
      break;
    default:
      ctx->errnum = IPMI_SDR_ERR_SYSTEM_ERROR;
    }
}

void
sdr_set_sdr_errnum_by_fiid_object (ipmi_sdr_ctx_t ctx, fiid_obj_t obj)
{
  assert (ctx && ctx->magic == IPMI_SDR_CTX_MAGIC);

  switch (fiid_obj_errnum (obj))
    {
    case FIID_ERR_SUCCESS:
      ctx->errnum = IPMI_SDR_ERR_SUCCESS;
      break;
    case FIID_ERR_OUT_OF_MEMORY:
      ctx->errnum = IPMI_SDR_ERR_OUT_OF_MEMORY;
      break;
    case FIID_ERR_DATA_NOT_AVAILABLE:
      ctx->errnum = IPMI_SDR_ERR_IPMI_ERROR;
      break;
    case FIID_ERR_FIELD_NOT_FOUND:
    case FIID_ERR_DATA_NOT_BYTE_ALIGNED:
    case FIID_ERR_REQUIRED_FIELD_MISSING:
    case FIID_ERR_FIXED_LENGTH_FIELD_INVALID:
    case FIID_ERR_NOT_IDENTICAL:
      ctx->errnum = IPMI_SDR_ERR_PARAMETERS;
      break;
    default:
      ctx->errnum = IPMI_SDR_ERR_INTERNAL_ERROR;
    }
}

void
sdr_set_internal_errnum (ipmi_sdr_ctx_t ctx)
{
  if (ctx->errnum == IPMI_SDR_ERR_CONTEXT_NULL
      || ctx->errnum == IPMI_SDR_ERR_CONTEXT_INVALID
      || ctx->errnum == IPMI_SDR_ERR_PARAMETERS
      || ctx->errnum == IPMI_SDR_ERR_OVERFLOW)
    ctx->errnum = IPMI_SDR_ERR_INTERNAL_ERROR; 
}
