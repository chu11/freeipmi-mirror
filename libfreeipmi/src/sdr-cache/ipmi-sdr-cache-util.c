/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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

#include "freeipmi/sdr-cache/ipmi-sdr-cache.h"
#include "freeipmi/fiid/fiid.h"

#include "ipmi-sdr-cache-defs.h"
#include "ipmi-sdr-cache-trace.h"
#include "ipmi-sdr-cache-util.h"

#include "freeipmi-portability.h"

void
sdr_cache_set_sdr_cache_errnum_by_errno (ipmi_sdr_cache_ctx_t ctx, int __errno)
{
  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    return;

  if (__errno == 0)
    ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  else if (__errno == ENOSPC)
    ctx->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
  else if (__errno == EMFILE)
    ctx->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
  else if (__errno == ENFILE)
    ctx->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
  else if (__errno == EPERM)
    ctx->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
  else if (__errno == EACCES)
    ctx->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
  else if (__errno == EISDIR)
    ctx->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
  else if (__errno == EROFS)
    ctx->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
  else if (__errno == ENOENT)
    ctx->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST;
  else if (__errno == ENOTDIR)
    ctx->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST;
  else if (__errno == ENAMETOOLONG)
    ctx->errnum = IPMI_SDR_CACHE_ERR_FILENAME_INVALID;
  else if (__errno == ELOOP)
    ctx->errnum = IPMI_SDR_CACHE_ERR_FILENAME_INVALID;
  else if (__errno == ENOMEM)
    ctx->errnum = IPMI_SDR_CACHE_ERR_OUT_OF_MEMORY;
  else if (__errno == EINVAL)
    ctx->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
  else
    ctx->errnum = IPMI_SDR_CACHE_ERR_SYSTEM_ERROR;
}

void
sdr_cache_set_sdr_cache_errnum_by_fiid_object (ipmi_sdr_cache_ctx_t ctx, fiid_obj_t obj)
{
  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    return;

  if (fiid_obj_errnum (obj) == FIID_ERR_SUCCESS)
    ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  else if (fiid_obj_errnum (obj) == FIID_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_SDR_CACHE_ERR_OUT_OF_MEMORY;
  else if (fiid_obj_errnum (obj) == FIID_ERR_DATA_NOT_AVAILABLE)
    ctx->errnum = IPMI_SDR_CACHE_ERR_IPMI_ERROR;
  else if (fiid_obj_errnum (obj) == FIID_ERR_FIELD_NOT_FOUND
           || fiid_obj_errnum (obj) == FIID_ERR_DATA_NOT_BYTE_ALIGNED
           || fiid_obj_errnum (obj) == FIID_ERR_REQUIRED_FIELD_MISSING
           || fiid_obj_errnum (obj) == FIID_ERR_FIXED_LENGTH_FIELD_INVALID
           || fiid_obj_errnum (obj) == FIID_ERR_DATA_NOT_AVAILABLE
           || fiid_obj_errnum (obj) == FIID_ERR_NOT_IDENTICAL)
    ctx->errnum = IPMI_SDR_CACHE_ERR_PARAMETERS;
  else
    ctx->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
}
