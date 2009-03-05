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
#include "freeipmi/locate/ipmi-locate.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/driver/ipmi-kcs-driver.h"
#include "freeipmi/driver/ipmi-openipmi-driver.h"
#include "freeipmi/driver/ipmi-ssif-driver.h"
#include "freeipmi/driver/ipmi-sunbmc-driver.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-api-defs.h"
#include "ipmi-api-util.h"
#include "ipmi-api-trace.h"

#include "freeipmi-portability.h"

void
api_set_api_errnum_by_errno (ipmi_ctx_t ctx, int __errno)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return;

  if (__errno == 0)
    ctx->errnum = IPMI_ERR_SUCCESS;
  else if (__errno == ENOMEM)
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
  else if (__errno == ENODEV)
    ctx->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;
  else
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
}

void
api_set_api_errnum_by_fiid_object (ipmi_ctx_t ctx, fiid_obj_t obj)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return;

  if (fiid_obj_errnum (obj) == FIID_ERR_SUCCESS)
    ctx->errnum = IPMI_ERR_SUCCESS;
  else if (fiid_obj_errnum (obj) == FIID_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
  else if (fiid_obj_errnum (obj) == FIID_ERR_DATA_NOT_AVAILABLE)
    ctx->errnum = IPMI_ERR_IPMI_ERROR;
  else
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
}

void
api_set_api_errnum_by_bad_response (ipmi_ctx_t ctx, fiid_obj_t obj_cmd_rs)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return;

  if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_NODE_BUSY) == 1
      || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OUT_OF_SPACE) == 1
      || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_SDR_UPDATE_MODE) == 1
      || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_FIRMWARE_UPDATE_MODE) == 1
      || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_BMC_INIT_MODE) == 1)
    ctx->errnum = IPMI_ERR_BMC_BUSY;
  else if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_COMMAND_INVALID) == 1
           || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_COMMAND_INVALID_FOR_LUN) == 1)
    ctx->errnum = IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND;
  else if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_REQUEST_DATA_TRUNCATED) == 1
           || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID) == 1
           || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_REQUEST_DATA_LENGTH_LIMIT_EXCEEDED) == 1
           || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE) == 1
           || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1
           || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1
           || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE) == 1
           || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_DESTINATION_UNAVAILABLE) == 1
           || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED) == 1
           || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_REQUEST_PARAMETER_ILLEGAL) == 1)
    ctx->errnum = IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID;
  else if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL) == 1)
    ctx->errnum = IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT;
  else
    ctx->errnum = IPMI_ERR_BAD_COMPLETION_CODE;
}

void
api_set_api_errnum_by_locate_errnum (ipmi_ctx_t ctx, int locate_errnum)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return;

  if (locate_errnum == IPMI_LOCATE_ERR_SUCCESS)
    ctx->errnum = IPMI_ERR_SUCCESS;
  else if (locate_errnum == IPMI_LOCATE_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
  else if (locate_errnum == IPMI_LOCATE_ERR_PERMISSION)
    ctx->errnum = IPMI_ERR_PERMISSION;
  else if (locate_errnum == IPMI_LOCATE_ERR_SYSTEM_ERROR)
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
  else
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
}

void
api_set_api_errnum_by_kcs_errnum (ipmi_ctx_t ctx, int kcs_errnum)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return;

  if (kcs_errnum == IPMI_KCS_ERR_SUCCESS)
    ctx->errnum = IPMI_ERR_SUCCESS;
  else if (kcs_errnum == IPMI_KCS_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
  else if (kcs_errnum == IPMI_KCS_ERR_PERMISSION)
    ctx->errnum = IPMI_ERR_PERMISSION;
  else if (kcs_errnum == IPMI_KCS_ERR_DEVICE_NOT_FOUND)
    ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;
  else if (kcs_errnum == IPMI_KCS_ERR_DRIVER_TIMEOUT)
    ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;
  else if (kcs_errnum == IPMI_KCS_ERR_BUSY)
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
  else if (kcs_errnum == IPMI_KCS_ERR_SYSTEM_ERROR)
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
  else
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
}

void
api_set_api_errnum_by_ssif_errnum (ipmi_ctx_t ctx, int ssif_errnum)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return;

  if (ssif_errnum == IPMI_SSIF_ERR_SUCCESS)
    ctx->errnum = IPMI_ERR_SUCCESS;
  else if (ssif_errnum == IPMI_SSIF_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
  else if (ssif_errnum == IPMI_SSIF_ERR_PERMISSION)
    ctx->errnum = IPMI_ERR_PERMISSION;
  else if (ssif_errnum == IPMI_SSIF_ERR_DEVICE_NOT_FOUND)
    ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;
  else if (ssif_errnum == IPMI_SSIF_ERR_DRIVER_TIMEOUT)
    ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;
  else if (ssif_errnum == IPMI_SSIF_ERR_BUSY)
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
  else if (ssif_errnum == IPMI_SSIF_ERR_SYSTEM_ERROR)
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
  else
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
}

void
api_set_api_errnum_by_openipmi_errnum (ipmi_ctx_t ctx, int openipmi_errnum)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return;

  if (openipmi_errnum == IPMI_OPENIPMI_ERR_SUCCESS)
    ctx->errnum = IPMI_ERR_SUCCESS;
  else if (openipmi_errnum == IPMI_OPENIPMI_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
  else if (openipmi_errnum == IPMI_OPENIPMI_ERR_PERMISSION)
    ctx->errnum = IPMI_ERR_PERMISSION;
  else if (openipmi_errnum == IPMI_OPENIPMI_ERR_DEVICE_NOT_FOUND)
    ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;
  else if (openipmi_errnum == IPMI_OPENIPMI_ERR_DRIVER_TIMEOUT)
    ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;
  else if (openipmi_errnum == IPMI_OPENIPMI_ERR_SYSTEM_ERROR)
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
  else
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
}

void
api_set_api_errnum_by_sunbmc_errnum (ipmi_ctx_t ctx, int sunbmc_errnum)
{
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return;

  if (sunbmc_errnum == IPMI_SUNBMC_ERR_SUCCESS)
    ctx->errnum = IPMI_ERR_SUCCESS;
  else if (sunbmc_errnum == IPMI_SUNBMC_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
  else if (sunbmc_errnum == IPMI_SUNBMC_ERR_PERMISSION)
    ctx->errnum = IPMI_ERR_PERMISSION;
  else if (sunbmc_errnum == IPMI_SUNBMC_ERR_DEVICE_NOT_FOUND)
    ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;
  else if (sunbmc_errnum == IPMI_SUNBMC_ERR_DRIVER_TIMEOUT)
    ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;
  else if (sunbmc_errnum == IPMI_SUNBMC_ERR_SYSTEM_ERROR)
    ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
  else
    ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
}

int
api_fiid_obj_packet_valid (ipmi_ctx_t ctx, fiid_obj_t obj)
{
  int ret;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return (-1);

  if ((ret = fiid_obj_packet_valid (obj)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj);
      return (-1);
    }

  if (!ret)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  return (1);                   /* return 1 like real call */
}

int
api_fiid_obj_template_compare (ipmi_ctx_t ctx, fiid_obj_t obj, fiid_template_t tmpl)
{
  int ret;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return (-1);

  if (!fiid_obj_valid (obj))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if (!fiid_obj_valid (obj))
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  if ((ret = fiid_obj_template_compare (obj, tmpl)) < 0)
    {
      API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj);
      return (-1);
    }

  if (!ret)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  return (1);                   /* return 1 like real call */
}

int
api_ipmi_cmd (ipmi_ctx_t ctx,
              uint8_t lun,
              uint8_t net_fn,
              fiid_obj_t obj_cmd_rq,
              fiid_obj_t obj_cmd_rs)
{
  int8_t ret;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return (-1);

  /* Note: ctx->errnum set in call to ipmi_cmd() */
  if (ipmi_cmd (ctx,
                lun,
                net_fn,
                obj_cmd_rq,
                obj_cmd_rs) < 0)
    return (-1);

  if ((ret = ipmi_check_completion_code_success (obj_cmd_rs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  if (!ret)
    {
      API_BAD_RESPONSE_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  return (0);
}

int
api_ipmi_cmd_ipmb (ipmi_ctx_t ctx,
                   uint8_t rs_addr,
                   uint8_t lun,
                   uint8_t net_fn,
                   fiid_obj_t obj_cmd_rq,
                   fiid_obj_t obj_cmd_rs)
{
  int8_t ret;

  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    return (-1);

  /* Note: ctx->errnum set in call to ipmi_cmd_ipmb() */
  if (ipmi_cmd_ipmb (ctx,
                     rs_addr,
                     lun,
                     net_fn,
                     obj_cmd_rq,
                     obj_cmd_rs) < 0)
    return (-1);

  if ((ret = ipmi_check_completion_code_success (obj_cmd_rs)) < 0)
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      return (-1);
    }

  if (!ret)
    {
      API_BAD_RESPONSE_TO_API_ERRNUM (ctx, obj_cmd_rs);
      return (-1);
    }

  return (0);
}
