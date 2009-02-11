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

#include "ipmi-sdr-cache-defs.h"
#include "ipmi-sdr-cache-trace.h"
#include "ipmi-sdr-cache-util.h"

#include "freeipmi-portability.h"

void
sdr_cache_set_sdr_cache_errnum_by_errno(ipmi_sdr_cache_ctx_t ctx, int __errno)
{
  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    return;

  if (errno == 0)
    ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  else if (errno == ENOSPC)
    ctx->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
  else if (errno == EMFILE)
    ctx->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
  else if (errno == ENFILE)
    ctx->errnum = IPMI_SDR_CACHE_ERR_FILESYSTEM;
  else if (errno == EPERM)
    ctx->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
  else if (errno == EACCES)
    ctx->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
  else if (errno == EISDIR)
    ctx->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
  else if (errno == EROFS)
    ctx->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
  else if (errno == ENOENT)
    ctx->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST;
  else if (errno == ENOTDIR)
    ctx->errnum = IPMI_SDR_CACHE_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST;
  else if (errno == ENAMETOOLONG)
    ctx->errnum = IPMI_SDR_CACHE_ERR_FILENAME_INVALID;
  else if (errno == ELOOP)
    ctx->errnum = IPMI_SDR_CACHE_ERR_FILENAME_INVALID;
  else if (errno == ENOMEM)
    ctx->errnum = IPMI_SDR_CACHE_ERR_OUT_OF_MEMORY;
  else if (errno == EINVAL)
    ctx->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
  else
    ctx->errnum = IPMI_SDR_CACHE_ERR_SYSTEM_ERROR;
}

void
sdr_cache_set_sdr_cache_errnum_by_fiid_object(ipmi_sdr_cache_ctx_t ctx, fiid_obj_t obj)
{
  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    return;

  if (!fiid_obj_valid(obj))
    {
      SDR_CACHE_SET_ERRNUM(ctx, IPMI_ERR_INTERNAL_ERROR);
      return;
    }

  if (fiid_obj_errnum(obj) == FIID_ERR_SUCCESS)
    ctx->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  else if (fiid_obj_errnum(obj) == FIID_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_SDR_CACHE_ERR_OUT_OF_MEMORY;
  else
    ctx->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
}

int
sdr_cache_fiid_obj_get(ipmi_sdr_cache_ctx_t ctx, fiid_obj_t obj, char *field, uint64_t *val)
{
  uint64_t lval;
  int ret;

  if (!ctx || ctx->magic != IPMI_SDR_CACHE_CTX_MAGIC)
    return (-1);

  if (!fiid_obj_valid(obj))
    {
      SDR_CACHE_SET_ERRNUM(ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((ret = fiid_obj_get(obj, field, &lval)) < 0)
    {
      SDR_CACHE_FIID_OBJECT_ERROR_TO_SDR_CACHE_ERRNUM(ctx, obj);
      return (-1);
    }

  if (!ret)
    {
      SDR_CACHE_SET_ERRNUM(ctx, IPMI_SDR_CACHE_ERR_IPMI_ERROR);
      return (-1);
    }

  *val = lval;
  return (0);
}
