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

#include "freeipmi/sensor-read/ipmi-sensor-read.h"

#include "ipmi-sensor-read-defs.h"
#include "ipmi-sensor-read-trace.h"
#include "ipmi-sensor-read-util.h"

#include "freeipmi-portability.h"

void
sensor_read_set_sensor_read_errnum_by_errno(ipmi_sensor_read_ctx_t ctx, int __errno)
{  
  if (!ctx || ctx->magic != IPMI_SENSOR_READ_CTX_MAGIC)
    return;

  if (__errno == 0)
    ctx->errnum = IPMI_SENSOR_READ_ERR_SUCCESS;
  else if (__errno == ENOMEM)
    ctx->errnum = IPMI_SENSOR_READ_ERR_OUT_OF_MEMORY;
  else
    ctx->errnum = IPMI_SENSOR_READ_ERR_INTERNAL_ERROR;
}

void
sensor_read_set_sensor_read_errnum_by_fiid_object(ipmi_sensor_read_ctx_t ctx, fiid_obj_t obj)
{
  if (!ctx || ctx->magic != IPMI_SENSOR_READ_CTX_MAGIC)
    return;

  if (!fiid_obj_valid(obj))
    {
      SENSOR_READ_SET_ERRNUM(ctx, IPMI_ERR_INTERNAL_ERROR);
      return;
    }

  if (fiid_obj_errnum(obj) == FIID_ERR_SUCCESS)
    ctx->errnum = IPMI_SENSOR_READ_ERR_SUCCESS;
  else if (fiid_obj_errnum(obj) == FIID_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_SENSOR_READ_ERR_OUT_OF_MEMORY;
  else
    ctx->errnum = IPMI_SENSOR_READ_ERR_INTERNAL_ERROR;
}

int
sensor_read_fiid_obj_get(ipmi_sensor_read_ctx_t ctx, fiid_obj_t obj, char *field, uint64_t *val)
{
  uint64_t lval;
  int ret;

  if (!ctx || ctx->magic != IPMI_SENSOR_READ_CTX_MAGIC)
    return (-1);

  if (!fiid_obj_valid(obj))
    {
      SENSOR_READ_SET_ERRNUM(ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((ret = fiid_obj_get(obj, field, &lval)) < 0)
    {
      SENSOR_READ_FIID_OBJECT_ERROR_TO_SENSOR_READ_ERRNUM(ctx, obj);
      return (-1);
    }

  if (!ret)
    {
      SENSOR_READ_SET_ERRNUM(ctx, IPMI_SENSOR_READ_ERR_IPMI_ERROR);
      return (-1);
    }

  *val = lval;
  return (0);
}
