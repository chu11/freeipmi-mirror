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

#include "freeipmi/sdr-parse/ipmi-sdr-parse.h"

#include "ipmi-sdr-parse-defs.h"
#include "ipmi-sdr-parse-trace.h"
#include "ipmi-sdr-parse-util.h"

#include "freeipmi-portability.h"

void
sdr_parse_set_sdr_parse_errnum_by_errno (ipmi_sdr_parse_ctx_t ctx, int __errno)
{
  if (!ctx || ctx->magic != IPMI_SDR_PARSE_CTX_MAGIC)
    return;

  if (__errno == 0)
    ctx->errnum = IPMI_SDR_PARSE_ERR_SUCCESS;
  else if (__errno == ENOMEM)
    ctx->errnum = IPMI_SDR_PARSE_ERR_OUT_OF_MEMORY;
  else
    ctx->errnum = IPMI_SDR_PARSE_ERR_INTERNAL_ERROR;
}

void
sdr_parse_set_sdr_parse_errnum_by_fiid_object (ipmi_sdr_parse_ctx_t ctx, fiid_obj_t obj)
{
  if (!ctx || ctx->magic != IPMI_SDR_PARSE_CTX_MAGIC)
    return;

  if (fiid_obj_errnum (obj) == FIID_ERR_SUCCESS)
    ctx->errnum = IPMI_SDR_PARSE_ERR_SUCCESS;
  else if (fiid_obj_errnum (obj) == FIID_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_SDR_PARSE_ERR_OUT_OF_MEMORY;
  else
    ctx->errnum = IPMI_SDR_PARSE_ERR_INTERNAL_ERROR;
}

int
sdr_parse_fiid_obj_get (ipmi_sdr_parse_ctx_t ctx, fiid_obj_t obj, char *field, uint64_t *val)
{
  uint64_t lval;
  int ret;

  if (!ctx || ctx->magic != IPMI_SDR_PARSE_CTX_MAGIC)
    return (-1);

  if ((ret = fiid_obj_get (obj, field, &lval)) < 0)
    {
      SDR_PARSE_FIID_OBJECT_ERROR_TO_SDR_PARSE_ERRNUM (ctx, obj);
      return (-1);
    }

  if (!ret)
    {
      SDR_PARSE_SET_ERRNUM (ctx, IPMI_SDR_PARSE_ERR_INCOMPLETE_SDR_RECORD);
      return (-1);
    }

  *val = lval;
  return (1);                   /* return 1 like real call */
}
