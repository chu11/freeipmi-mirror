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

#include "freeipmi/api/ipmi-api.h"
#include "freeipmi/fiid/fiid.h"

#include "ipmi-ctx.h"
#include "ipmi-err-wrappers-api.h"
#include "ipmi-fiid-wrappers-api.h"

#include "freeipmi-portability.h"

void
ipmi_set_api_errnum_by_fiid_object(ipmi_ctx_t ctx, fiid_obj_t obj)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return;

  if (!fiid_obj_valid(obj))
    {
      API_SET_ERRNUM(IPMI_ERR_INTERNAL_ERROR);
      return;
    }

  if (fiid_obj_errnum(obj) == FIID_ERR_SUCCESS)
    ctx->errnum = IPMI_ERR_SUCCESS;
  else if (fiid_obj_errnum(obj) == FIID_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
  else
    ctx->errnum = IPMI_ERR_LIBRARY_ERROR;
}

int
api_fiid_obj_packet_valid(ipmi_ctx_t ctx, fiid_obj_t obj)
{
  int ret;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return (-1);

  if ((ret = fiid_obj_valid(obj)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM(ctx, obj);
      return (-1);
    }

  if (!ret)
    {
      ctx->errnum = IPMI_ERR_PARAMETERS;
      return (-1);
    }

  return (0);
}

int
api_fiid_obj_template_compare(ipmi_ctx_t ctx, fiid_obj_t obj, fiid_template_t tmpl)
{
  int ret;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return (-1);

  if (!fiid_obj_valid(obj))
    {
      API_SET_ERRNUM(IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (!fiid_obj_valid(obj))
    {
      API_SET_ERRNUM(IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }
 
  if ((ret = fiid_obj_template_compare (obj, tmpl)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM(ctx, errno);
      return (-1);
    }

  if (!ret)
    {
      ctx->errnum = IPMI_ERR_PARAMETERS;
      return (-1);
    }

  return (0);
}

