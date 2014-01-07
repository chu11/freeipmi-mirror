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

#include "freeipmi/api/ipmi-api.h"
#include "freeipmi/locate/ipmi-locate.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/driver/ipmi-inteldcmi-driver.h"
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
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  switch (__errno)
    {
    case 0:
      ctx->errnum = IPMI_ERR_SUCCESS;
      break;
    case ENOMEM:
      ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
      break;
    case ENODEV:
      ctx->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;
      break;
    case ECONNRESET:
      ctx->errnum = IPMI_ERR_IPMI_ERROR;
      break;
    case ECONNREFUSED:
      ctx->errnum = IPMI_ERR_IPMI_ERROR;
      break;
    case EINVAL:
      ctx->errnum = IPMI_ERR_PARAMETERS;
      break;
    default:
      ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
    }
}

void
api_set_api_errnum_by_fiid_object (ipmi_ctx_t ctx, fiid_obj_t obj)
{
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  switch (fiid_obj_errnum (obj))
    {
    case FIID_ERR_SUCCESS:
      ctx->errnum = IPMI_ERR_SUCCESS;
      break;
    case FIID_ERR_OUT_OF_MEMORY:
      ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
      break;
    case FIID_ERR_DATA_NOT_AVAILABLE:
      ctx->errnum = IPMI_ERR_IPMI_ERROR;
      break;
    case FIID_ERR_FIELD_NOT_FOUND:
    case FIID_ERR_DATA_NOT_BYTE_ALIGNED:
    case FIID_ERR_REQUIRED_FIELD_MISSING:
    case FIID_ERR_FIXED_LENGTH_FIELD_INVALID:
    case FIID_ERR_NOT_IDENTICAL:
      ctx->errnum = IPMI_ERR_PARAMETERS;
      break;
    default:
      ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
    }
}

void
api_set_api_errnum_by_bad_response (ipmi_ctx_t ctx, fiid_obj_t obj_cmd_rs)
{
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  /* IPMI_COMP_CODE_COMMAND_TIMEOUT, assumes it's a IPMB or command
   * specific timeout, so set to "MESSAGE_TIMEOUT" so user can
   * continue on if they wish.  At minimum, returned by openipmi
   * driver for (what seems to be) collection of potential errors.
   */
  if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_COMMAND_TIMEOUT) == 1)
    ctx->errnum = IPMI_ERR_MESSAGE_TIMEOUT;
  else if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_NODE_BUSY) == 1
	   || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_OUT_OF_SPACE) == 1
	   || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_SDR_REPOSITORY_IN_UPDATE_MODE) == 1
	   || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_DEVICE_IN_FIRMWARE_UPDATE_MODE) == 1
	   || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_BMC_INITIALIZATION_IN_PROGRESS) == 1)
    ctx->errnum = IPMI_ERR_BMC_BUSY;
  else if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL) == 1)
    ctx->errnum = IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT;
  else if (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_INVALID_COMMAND) == 1
	   || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_COMMAND_INVALID_FOR_LUN) == 1
	   || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_REQUEST_DATA_LENGTH_INVALID) == 1
	   || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_REQUEST_DATA_LENGTH_LIMIT_EXCEEDED) == 1
	   || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE) == 1
	   || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1)
    ctx->errnum = IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED;
  else
    ctx->errnum = IPMI_ERR_BAD_COMPLETION_CODE;
}

void
api_set_api_errnum_by_locate_errnum (ipmi_ctx_t ctx, int locate_errnum)
{
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  switch (locate_errnum)
    {
    case IPMI_LOCATE_ERR_SUCCESS:
      ctx->errnum = IPMI_ERR_SUCCESS;
      break;
    case IPMI_LOCATE_ERR_OUT_OF_MEMORY:
      ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
      break;
    case IPMI_LOCATE_ERR_PERMISSION:
      ctx->errnum = IPMI_ERR_PERMISSION;
      break;
    case IPMI_LOCATE_ERR_SYSTEM_ERROR:
      ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
      break;
    default:
      ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
    }
}

void
api_set_api_errnum_by_kcs_errnum (ipmi_ctx_t ctx, int kcs_errnum)
{
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  switch (kcs_errnum)
    {
    case IPMI_KCS_ERR_SUCCESS:
      ctx->errnum = IPMI_ERR_SUCCESS;
      break;
    case IPMI_KCS_ERR_OUT_OF_MEMORY:
      ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
      break;
    case IPMI_KCS_ERR_PERMISSION:
      ctx->errnum = IPMI_ERR_PERMISSION;
      break;
    case IPMI_KCS_ERR_DEVICE_NOT_FOUND:
      ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;
      break;
    case IPMI_KCS_ERR_DRIVER_TIMEOUT:
      ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;
      break;
    case IPMI_KCS_ERR_BUSY:
      ctx->errnum = IPMI_ERR_DRIVER_BUSY;
      break;
    case IPMI_KCS_ERR_SYSTEM_ERROR:
      ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
      break;
    default:
      ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
    }
}

void
api_set_api_errnum_by_ssif_errnum (ipmi_ctx_t ctx, int ssif_errnum)
{
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  switch (ssif_errnum)
    {
    case IPMI_SSIF_ERR_SUCCESS:
      ctx->errnum = IPMI_ERR_SUCCESS;
      break;
    case IPMI_SSIF_ERR_OUT_OF_MEMORY:
      ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
      break;
    case IPMI_SSIF_ERR_PERMISSION:
      ctx->errnum = IPMI_ERR_PERMISSION;
      break;
    case IPMI_SSIF_ERR_DEVICE_NOT_FOUND:
      ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;
      break;
    case IPMI_SSIF_ERR_DRIVER_TIMEOUT:
      ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;
      break;
    case IPMI_SSIF_ERR_BUSY:
      ctx->errnum = IPMI_ERR_DRIVER_BUSY;
      break;
    case IPMI_SSIF_ERR_SYSTEM_ERROR:
      ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
      break;
    default:
      ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
    }
}

void
api_set_api_errnum_by_openipmi_errnum (ipmi_ctx_t ctx, int openipmi_errnum)
{
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  switch (openipmi_errnum)
    {
    case IPMI_OPENIPMI_ERR_SUCCESS:
      ctx->errnum = IPMI_ERR_SUCCESS;
      break;
    case IPMI_OPENIPMI_ERR_OUT_OF_MEMORY:
      ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
      break;
    case IPMI_OPENIPMI_ERR_PERMISSION:
      ctx->errnum = IPMI_ERR_PERMISSION;
      break;
    case IPMI_OPENIPMI_ERR_DEVICE_NOT_FOUND:
      ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;
      break;
    case IPMI_OPENIPMI_ERR_DRIVER_TIMEOUT:
      ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;
      break;
    case IPMI_OPENIPMI_ERR_SYSTEM_ERROR:
      ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
      break;
    default:
      ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
    }
}

void
api_set_api_errnum_by_sunbmc_errnum (ipmi_ctx_t ctx, int sunbmc_errnum)
{
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  switch (sunbmc_errnum)
    {
    case IPMI_SUNBMC_ERR_SUCCESS:
      ctx->errnum = IPMI_ERR_SUCCESS;
      break;
    case IPMI_SUNBMC_ERR_OUT_OF_MEMORY:
      ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
      break;
    case IPMI_SUNBMC_ERR_PERMISSION:
      ctx->errnum = IPMI_ERR_PERMISSION;
      break;
    case IPMI_SUNBMC_ERR_DEVICE_NOT_FOUND:
      ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;
      break;
    case IPMI_SUNBMC_ERR_DEVICE_NOT_SUPPORTED:
      ctx->errnum = IPMI_ERR_DEVICE_NOT_SUPPORTED;
      break;
    case IPMI_SUNBMC_ERR_DRIVER_TIMEOUT:
      ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;
      break;
    case IPMI_SUNBMC_ERR_SYSTEM_ERROR:
      ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
      break;
    default:
      ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
    }
}

void
api_set_api_errnum_by_inteldcmi_errnum (ipmi_ctx_t ctx, int inteldcmi_errnum)
{
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  switch (inteldcmi_errnum)
    {
    case IPMI_INTELDCMI_ERR_SUCCESS:
      ctx->errnum = IPMI_ERR_SUCCESS;
      break;
    case IPMI_INTELDCMI_ERR_OUT_OF_MEMORY:
      ctx->errnum = IPMI_ERR_OUT_OF_MEMORY;
      break;
    case IPMI_INTELDCMI_ERR_PERMISSION:
      ctx->errnum = IPMI_ERR_PERMISSION;
      break;
    case IPMI_INTELDCMI_ERR_DEVICE_NOT_FOUND:
      ctx->errnum = IPMI_ERR_DEVICE_NOT_FOUND;
      break;
    case IPMI_INTELDCMI_ERR_DRIVER_TIMEOUT:
      ctx->errnum = IPMI_ERR_DRIVER_TIMEOUT;
      break;
    case IPMI_INTELDCMI_ERR_SYSTEM_ERROR:
      ctx->errnum = IPMI_ERR_SYSTEM_ERROR;
      break;
    default:
      ctx->errnum = IPMI_ERR_INTERNAL_ERROR;
    }
}

static int
_api_ipmi_cmd_post (ipmi_ctx_t ctx, fiid_obj_t obj_cmd_rs)
{
  int ret;

  assert (ctx
	  && ctx->magic == IPMI_CTX_MAGIC
	  && fiid_obj_valid (obj_cmd_rs));

  if (ctx->flags & IPMI_FLAGS_NO_LEGAL_CHECK)
    {
      uint64_t val;

      /* Do not check completion code if data not available
       * (i.e. FIID_ERR_DATA_NOT_AVAILABLE completion code).
       *
       * Fallthrough to normal error if it's an alternate fiid error
       * (invalid packet, field not found, etc.)
       */
      
      if (FIID_OBJ_GET (obj_cmd_rs, "comp_code", &val) < 0)
	{
	  if (fiid_obj_errnum (obj_cmd_rs) == FIID_ERR_DATA_NOT_AVAILABLE)
	    goto skip_comp_code_check;
	}
    }

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

 skip_comp_code_check:

  if (!(ctx->flags & IPMI_FLAGS_NO_VALID_CHECK)
      && !(ctx->flags & IPMI_FLAGS_NO_LEGAL_CHECK))
    {
      if ((ret = fiid_obj_packet_valid (obj_cmd_rs)) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          return (-1);
        }

      if (!ret)
        {
          API_SET_ERRNUM (ctx, IPMI_ERR_IPMI_ERROR);
          return (-1);
        }
    }

  return (0);
}

int
api_ipmi_cmd (ipmi_ctx_t ctx,
              uint8_t lun,
              uint8_t net_fn,
              fiid_obj_t obj_cmd_rq,
              fiid_obj_t obj_cmd_rs)
{
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  /* Note: ctx->errnum set in call to ipmi_cmd() */
  if (ipmi_cmd (ctx,
                lun,
                net_fn,
                obj_cmd_rq,
                obj_cmd_rs) < 0)
    return (-1);

  return (_api_ipmi_cmd_post (ctx, obj_cmd_rs));
}

int
api_ipmi_cmd_ipmb (ipmi_ctx_t ctx,
                   uint8_t channel_number,
                   uint8_t rs_addr,
                   uint8_t lun,
                   uint8_t net_fn,
                   fiid_obj_t obj_cmd_rq,
                   fiid_obj_t obj_cmd_rs)
{
  assert (ctx && ctx->magic == IPMI_CTX_MAGIC);

  /* Note: ctx->errnum set in call to ipmi_cmd_ipmb() */
  if (ipmi_cmd_ipmb (ctx,
                     channel_number,
                     rs_addr,
                     lun,
                     net_fn,
                     obj_cmd_rq,
                     obj_cmd_rs) < 0)
    return (-1);

  return (_api_ipmi_cmd_post (ctx, obj_cmd_rs));
}
